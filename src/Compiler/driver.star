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
  import star.compiler.parser.
  import star.compiler.location.
  import star.compiler.term.repo.
  import star.compiler.types.
  import star.compiler.normalize.
  import star.compiler.data.

  public _main:(cons[string])=>().
  _main(Args) => valof{
    WI=_optval(parseUri("file:"++_cwd()));
    RI=_optval(parseUri("file:"++_repo()));
    try{
      valis handleCmds(processOptions(Args,[wdOption,
	    stdinOption,
	    repoOption,
	    graphOption,
	    traceDependencyOption,
	    traceAstOption,
	    showMacroOption,
	    traceMacroOption,
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
	    traceInlineOption],
	  defltOptions(WI,RI)
	))
    } catch string in {
      Msg => { logMsg(Msg);
	valis ()
      }
    };
  }

  handleCmds:((compilerOptions,cons[string]))=>().
  handleCmds((Opts,Args)) => valof{
    Repo = openupRepo(Opts.repo,Opts.cwd);
    
    if CatUri ?= parseUri("catalog") && CatU ?= resolveUri(Opts.cwd,CatUri) &&
	Cat ?= loadCatalog(CatU) then{
	  for P in Args do{
	    resetErrors();
	    Sorted = makeGraph(extractPkgSpec(P),Repo,Cat);

	    if traceDependencies! then
	      logMsg("Process packages in $(Sorted)");

	    if Grph ?= Opts.graph then {
	      putResource(Grph,makeDotGraph(P,Sorted))
	    };
	      
	    processPkgs(Sorted,Repo,Cat)
	  }
	}
    else{
      logMsg("could not access catalog")
    };
    valis ()
  }

  extractPkgSpec(P) where Lc ?= strFind(P,":",0) => .pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  extractPkgSpec(P) default => .pkg(P,.defltVersion).

  processPkg:(pkg,termRepo,catalog) => termRepo.
  processPkg(P,Repo,Cat) => valof{
    logMsg("Compiling $(P)");
    if (SrcUri,CPkg) ?= resolveInCatalog(Cat,pkgName(P)) then{
      if Ast ?=parseSrc(SrcUri,CPkg) then{
	if traceAst! then{
	  logMsg("Ast of $(P) is $(Ast)")
	};
	M = macroPkg(Ast);
	if showMacrod! then{
	  logMsg("Macroed package $(M)")
	};
	
	if errorFree() && ~ macroOnly! then{
	  (PkgSpec,Defs,IDecls,Decls) = checkPkg(Repo,CPkg,M);
	  if showCanon! then {
	    logMsg("$(PkgSpec)");
	    logMsg("type checked definitions #(displayDefs(Defs))");
	  };
	  
	  if errorFree() && ~ typeCheckOnly! then {
	    AllDecls = IDecls++Decls;
	    N = normalize(PkgSpec,Defs,AllDecls);
	    validProg(N,AllDecls);
	    
	    Inlined = ( optimization! ==.inlining ?? valof{
		if traceNormalize! then{
		  logMsg("pre-inlined code $(N)");
		};
		valis simplifyDefs(N);
	      } || N);
	    if showNormalize! then{
	      logMsg("normalized code #(dispCrProg(Inlined))");
	    };
	    validProg(Inlined,AllDecls);
	    if errorFree() && genCode! then{
	      Segs = compProg(P,Inlined,AllDecls);
	      
	      if showCode! then{
		logMsg("Generated code:");
		for Sg in Segs do{
		  showMsg("$(Sg)");
		}
	      };
	      PkgSig = mkTpl([pkgTerm(CPkg),
		  mkTpl(PkgSpec.imports//(.pkgImp(_,_,IPkg))=>pkgTerm(IPkg)),
		  mkTpl(PkgSpec.exports//((D)=>D::data))])::string;
	      
	      Code = mkTpl([pkgTerm(CPkg),
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
	    logMsg("no code generated");
	  }
	};
	if ~errorFree() then{
	  logMsg("$(countErrors()+countTraps()) errors found");
	};
	if ~warningFree() then
	  logMsg("$(countWarnings()) warnings found");
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
	logMsg("$(P) #(pkgOkInRepo(Repo,P,Imps) ?? "does not" || "needs") recompiling");
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
}
