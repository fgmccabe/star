star.compiler{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.resources.
  import star.file.
  import star.uri.

  import star.repo.

  import star.compiler.ast.
  import star.compiler.assem.
  import star.compiler.canon.
  import star.compiler.catalog.
  import star.compiler.checker.
  import star.compiler.encode.
  import star.compiler.grapher.
  import star.compiler.term.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.gencode.
  import star.compiler.impawt.
  import star.compiler.inline.
  import star.compiler.macro.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.opts.
  import star.compiler.parser.
  import star.compiler.location.
  import star.compiler.package.merge.
  import star.compiler.term.repo.
  import star.compiler.types.
  import star.compiler.normalize.
  import star.compiler.data.
--  import star.compiler.wasm.types.
--  import star.compiler.wasm.gentypes.
--  import star.compiler.wasm.gen.

  public _main:(cons[string])=>().
  _main(Args) => valof{
    try{
      WI= ? parseUri("file:"++_cwd());
      RI= ? parseUri("file:"++_repo());
      try{
	valis handleCmds(processOptions(Args,[wdOption,
	      stdinOption,
	      repoOption,
	      graphOption,
	      forceCompileOption,
	      traceDependencyOption,
	      traceAstOption,
	      traceResolveOption,
	      showMacroOption,
	      macroTraceOption,
	      checkOnlyOption,
	      showCheckOption,
	      traceCheckOption,
	      macroOnlyOption,
	      showNormalizeOption,
	      traceNormalizeOption,
	      noCodeOption,
	      showCodegenOption,	    
	      traceCodegenOption,	    
	      optimizeLvlOption,
	      traceInlineOption,
	      genDebugOption,
	      genWasmOption,
	      traceWasmOption
	    ],
	    defltOptions(WI,RI)
	  ))
      } catch {
	Msg => {
	  logMsg(.severe,Msg);
	  valis ()
	}
      }
    } catch {
      .exception(Msg) => {
	logMsg(.severe,Msg);
	valis ()
      }
    }
  }

  handleCmds:((compilerOptions,cons[string]))=>().
  handleCmds((Opts,Args)) => valof{
    Repo = openupRepo(Opts.repo,Opts.cwd);
    
    if CatUri ?= parseUri("catalog") && CatU ?= resolveUri(Opts.cwd,CatUri) &&
	Cat ?= loadCatalog(CatU) then{
      if forceCompile! then{
	forceProcessPkgs(Args//extractPkgSpec,Repo,Cat);
      }
      else{
	for P in Args do{
	  resetErrors();
	  Sorted = makeGraph(extractPkgSpec(P),Repo,Cat);

	  if traceDependencies! then
	    showMsg("Process packages in $(Sorted)");

	  if Grph ?= Opts.graph then {
	    putResource(Grph,makeDotGraph(P,Sorted))
	  };

	  processPkgs(Sorted,Repo,Cat)
	}
      }
	}
    else{
      logMsg(.severe,"could not access catalog")
    };
    valis ()
  }

  extractPkgSpec(P) where Lc ?= strFind(P,":",0) => .pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  extractPkgSpec(P) default => .pkg(P,.defltVersion).

  processPkg:(pkg,termRepo,catalog) => termRepo.
  processPkg(P,Repo,Cat) => valof{
    logMsg(.info,"Compiling $(P)");
    if (SrcUri,CPkg) ?= resolveInCatalog(Cat,pkgName(P)) then{
      if Ast ?=parseSrc(SrcUri,CPkg) then{
	if traceAst! then{
	  showMsg("Ast of $(P) is $(Ast)")
	};
	M = macroPkg(Ast);
	if showMacrod! then{
	  showMsg("Macroed package $(M)")
	};
	
	if errorFree() && ~ macroOnly! then{
	  (PkgSpec,Defs,IDecls,Decls) = checkPkg(Repo,CPkg,M);
	  if showCanon! then {
	    showMsg("type checked definitions #(displayDefs(Defs))");
	  };
	  
	  if errorFree() && ~ typeCheckOnly! then {
	    AllDecls = IDecls++Decls;
	    N = normalize(PkgSpec,Defs,AllDecls);
	    
	    Inlined = valof{
	      if optimization! ==.inlining && mainDefined(AllDecls) then{
		(Imported,Merged) = mergePkgs(
		  PkgSpec.imports//(.pkgImp(_,_,IPkg))=>IPkg,
		  .some(pkgLoc(P)),Repo,[P],[]);
		if traceInline! then
		  showMsg("Merged definitions $(Merged)");
		valis simplifyDefs(Merged,N)
	      } else
	      valis N};
	    if showNormalize! then{
	      showMsg("normalized code #(dispCrProg(Inlined))");
	    };
	    if .base .= optimization! then
	      validProg(Inlined,AllDecls);
	    -- if errorFree() && genWasm! then{
	    --   (Imported,Merged) = mergePkgs(
	    -- 	PkgSpec.imports//(.pkgImp(_,_,IPkg))=>IPkg
	    -- 	,.some(pkgLoc(P)),Repo,[P],Inlined);
	    --   tpMap = buildWasmTypeMap(Merged);
	    --   if traceWasm! then{
	    -- 	showMsg("wasm type map: $(tpMap)");
	    --   };
	    -- };
	    if errorFree() && genCode! then{
	      Segs = compProg(P,Inlined,AllDecls);
	      
	      if showCode! then{
		showMsg("Generated code:");
		for Sg in Segs do{
		  showMsg("$(Sg)");
		}
	      };
	      PkgSig = mkTpl([pkgTerm(CPkg),
		  mkTpl(PkgSpec.imports//(.pkgImp(_,_,IPkg))=>pkgTerm(IPkg)),
		  mkTpl(PkgSpec.exports//((D)=>D::data))])::string;

	      Code = mkTpl([pkgTerm(CPkg),
		  .intgr(opcodeHash),
		  mkTpl(PkgSpec.imports//(.pkgImp(_,_,IPkg))=>pkgTerm(IPkg)),
		  mkTpl([]),		
		  mkTpl([]),
		  mkTpl(Segs//assem)]);
	      Bytes = (.strg(Code::string)::string);
	      
	      InlineBytes = (mkTpl(Inlined//((I)=>freezeDefn(I))))::string;
	      
	      if errorFree() then{
		valis addSpec(PkgSpec,
		  addSource(addLoweredSource(
		      addPackage(Repo,CPkg,Bytes),CPkg,InlineBytes),CPkg,SrcUri::string))
	      }
	    } else
	    showMsg("no code generated");
	  }
	};
	if ~errorFree() then{
	  showMsg("$(countErrors()+countTraps()) errors found");
	};
	if ~warningFree() then
	  showMsg("$(countWarnings()) warnings found");
	valis Repo
      }
      else{
	reportError("could not parse source $(SrcUri)",.none);
	valis Repo
      }
    }
    else{
      reportError("cannot locate source of $(P)",.some(pkgLoc(P)));
      valis Repo
    }
  }

  openupRepo:(uri,uri) => termRepo.
  openupRepo(RU,CU) where CRU ?= resolveUri(CU,RU) => openRepository(CRU).

  addSpec:(pkgSpec,termRepo) => termRepo.
  addSpec(Spec,R) => addSigToRepo(R,Spec.pkg,(Spec::data)::string).

  processPkgs:(cons[(pkg,cons[pkg])],termRepo,catalog) => ().
  processPkgs(Pks,Repo,Cat) => valof{
    Repp = ref Repo;

    pkgLoop: for (P,Imps) in Pks do{
      if traceDependencies! then
	showMsg("$(P) #(pkgOkInRepo(Repo,P,Imps) ?? "does not" || "needs") recompiling");
      if ~pkgOkInRepo(Repo,P,Imps) then{
	Repp := processPkg(P,Repp!,Cat);
	if ~errorFree() then{
	  break pkgLoop
	}
      }
    };
    flushRepo(Repp!);
    valis ()
  }

  forceProcessPkgs:(cons[pkg],termRepo,catalog) => ().
  forceProcessPkgs(Pks,Repo,Cat) => valof{
    Repp := Repo;
    pkgLoop:for P in Pks do{
      resetErrors();
      Repp := processPkg(P,Repp!,Cat);
      if ~errorFree() then{
	break pkgLoop
      }
    };
    flushRepo(Repp!);
    valis ()
  }
}
