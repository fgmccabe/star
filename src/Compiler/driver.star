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
    WI=^parseUri("file:"++_cwd());
    RI=^parseUri("file:"++_repo());
    try{
      valis handleCmds(processOptions(Args,[wdOption,
	    stdinOption,
	    repoOption,
	    graphOption,
	    traceDependencyOption,
	    traceAstOption,
	    macroTracingOption,
	    checkOnlyOption,
	    showCheckOption,
	    traceCheckOption,
	    macroOnlyOption,
	    showNormalizeOption,
	    traceNormalizeOption,
	    noCodeOption,
	    showCodegenOption,	    
	    traceCodegenOption,	    
	    optimizeLvlOption],
	  defltOptions(WI,RI)
	))
    } catch {
      Msg => { logMsg(Msg);
	valis ()
      }
    };
  }.

  handleCmds:((compilerOptions,cons[string]))=>().
  handleCmds((Opts,Args)) => valof{
    Repo = openupRepo(Opts.repo,Opts.cwd);
    
    if CatUri ?= parseUri("catalog") && CatU ?= resolveUri(Opts.cwd,CatUri) &&
	Cat ?= loadCatalog(CatU) then{
	  for P in Args do{
	    resetErrors();
	    Sorted = makeGraph(extractPkgSpec(P),Repo,Cat);

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

  extractPkgSpec(P) where Lc ?= strFind(P,":",0) => pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  extractPkgSpec(P) default => pkg(P,.defltVersion).

  processPkg:(pkg,termRepo,catalog) => termRepo.
  processPkg(P,Repo,Cat) => valof{
    logMsg("Compiling $(P)");
    if (SrcUri,CPkg) ?= resolveInCatalog(Cat,pkgName(P)) then{
      Ast = ^parseSrc(SrcUri,CPkg);
      if traceAst! then{
	logMsg("Ast of $(P) is $(Ast)")
      };
      M = macroPkg(Ast);
      if macroTracing! then{
	logMsg("Macroed package $(M)")
      };

      if errorFree() && ~ macroOnly! then{
	(PkgSpec,Defs,IDecls,Decls) = checkPkg(Repo,CPkg,M);
	if showCanon! then {
	  logMsg("type checked #(displayDefs(Defs))")
	};

	if errorFree() && ~ typeCheckOnly! then {
	  N = normalize(PkgSpec,Defs,Decls);
	  validProg(N,IDecls++Decls);
	  if traceNormalize! then{
	    logMsg("normalized code $(N)");
	  };
	  Inlined = ( optimization! ==.inlining ? simplifyDefs(N) || N);
	  validProg(Inlined,IDecls++Decls);
	  if showNormalize! then{
	    logMsg("normalized code $(Inlined)");
	  };
	  if errorFree() && genCode! then{
	    Segs = compProg(P,Inlined,Decls);

	    if showCode! then{
	      logMsg("Generated code:");
	      for Sg in Segs do{
	        showMsg("$(Sg)");
	      }
	    };
	    PkgSig = mkTpl([pkgTerm(CPkg),
		mkTpl(PkgSpec.imports//(.pkgImp(_,_,IPkg))=>pkgTerm(IPkg)),
		mkTpl(PkgSpec.exports//((D)=>D::data))])::string;
	    logMsg("pkg sig $(PkgSig)");

	    Code = mkTpl(Segs//assem);
	    Bytes = (strg(Code::string)::string);

	    valis addSource(addPackage(Repo,CPkg,Bytes),CPkg,SrcUri::string)
	  }
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
      reportError("cannot locate source of $(P)",some(pkgLoc(P)));
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

    for (P,Imps) in Pks do{
--      logMsg("is $(P) ok? $(pkgOk(Repo,P))");
      if ~ {? (pkgOk(Repo,P) && I in Imps *> pkgOk(Repo,I)) ?} then{
	Repp := processPkg(P,Repp!,Cat);
      }
    };
    flushRepo(Repp!);
    valis ()
  }
}
