star.compiler{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.resources.
  import star.file.
  import star.uri.

  import star.repo.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.catalog.
  import star.compiler.checker.
  import star.compiler.core.
  import star.compiler.dict.
  import star.compiler.errors.
--  import star.compiler.gencode.
  import star.compiler.grapher.
  import star.compiler.impawt.
  import star.compiler.inline.
  import star.compiler.macro.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.normalize.
  import star.compiler.parser.
  import star.compiler.location.
  import star.compiler.term.repo.
  import star.compiler.terms.
  import star.compiler.types.

  public _main:(cons[string])=>().
  _main(Args) => valof{
    WI^=parseUri("file:"++_cwd());
    RI^=parseUri("file:"++_repo());
    handleCmds(processOptions(Args,[wdOption,
	  stdinOption,
	  repoOption,
	  graphOption,
	  traceDependencyOption,
	  traceAstOption,
	  traceMacroOption,
	  checkOnlyOption,
	  traceCheckOption,
	  traceNormalizeOption,
	  macroOnlyOption,
	  optimizeLvlOption],
	defltOptions(WI,RI)
      ))
  }.

  handleCmds:(either[string,(compilerOptions,cons[string])])=>result[(),()].
  handleCmds(either((Opts,Args))) => do{
    Repo <- openupRepo(Opts.repo,Opts.cwd);
    
    if CatUri ^= parseUri("catalog") && CatU ^= resolveUri(Opts.cwd,CatUri) &&
	Cat ^= loadCatalog(CatU) then{
	  for P in Args do{
	    ErRp .= reports([]);

	    try{
	      Sorted <- makeGraph(extractPkgSpec(P),Repo,Cat,ErRp);

	      if Grph ^= Opts.graph then {
		ignore putResource(Grph,makeDotGraph(P,Sorted))
	      };
	      
	      processPkgs(Sorted,Repo,Cat,Opts,ErRp)
	    } catch (Er) => do{
	      logMsg("$(Er)");
	      valis _exit(9);
	    }
	  }
	}
    else{
      logMsg("could not access catalog")
    }
  }
  handleCmds(other(Msg)) => do{
    logMsg(Msg)
  }

  extractPkgSpec(P) where Lc ^= strFind(P,":",0) => pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  extractPkgSpec(P) default => pkg(P,.defltVersion).

  implementation all e,k ~~ coercion[(option[k],e),result[e,k]] => {
    _coerce((.none,R)) => some(bad(R)).
    _coerce((some(A),_)) => some(ok(A)).
  }

  implementation all e,k ~~ coercion[either[e,k],result[e,k]] => {
    _coerce(either(E)) => some(ok(E)).
    _coerce(other(A)) => some(bad(A)).
  }

  processPkg:(pkg,termRepo,catalog,compilerOptions,reports) => result[reports,termRepo].
  processPkg(P,Repo,Cat,Opts,Rp) => do{
    if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(P)) then{
      Ast <- parseSrc(SrcUri,CPkg,Rp)::result[reports,ast];
      if traceAst! then{
	logMsg("Ast of $(P) is $(Ast)")
      };
      M <- macroPkg(Ast,Rp);
      if traceMacro! then{
	logMsg("Macroed package $(M)")
      };

      if ~ macroOnly! then{
	(PkgSpec,Defs,Decls) <- checkPkg(Repo,CPkg,M,Opts,Rp);
	if traceCanon! then {
	  logMsg("type checked $(Defs)")
	};
	if ~ typeCheckOnly! then {
	  NormDefs <- normalize(PkgSpec,Defs,Decls,Rp);
	  if traceNormalize! then{
	    logMsg("normalized code $(NormDefs)");
	  };

	  Inlined .= ( optimization! ==.inlining ? simplifyDefs(NormDefs) || NormDefs);
	  if traceNormalize! then{
	    logMsg("inlined code $(Inlined)");
	  };

	  Repo1 .= addSpec(PkgSpec,Repo);
	  if traceNormalize! then{
	    logMsg("normalized code $(Inlined)");
	  };
	  valis Repo1

/*
	  if genCode! then{
	    Ins <- compCrProg(P,Inlined,pkgDecls(PkgSpec),Opts,Rp) :: action[reports,cons[codeSegment]];
	    if showCode! then
	      logMsg("Generated instructions $(Ins)");
	    Code .= mkTpl([pkgTerm(CPkg),strg(encodeSignature(typeOf(PkgSpec))),
		mkTpl(pkgImports(PkgSpec)//(pkgImp(_,_,IPkg))=>pkgTerm(IPkg)),
		mkTpl(Ins//assem)]);
	    Bytes .= (strg(Code::string)::string);
	    Repp := addSource(addPackage(Repp!,CPkg,Bytes),CPkg,SrcUri::string)
	  }
*/
	} else
	valis Repo
      }
      else
      valis Repo
    }
    else
    raise reportError(Rp,"cannot locate source of $(P)",some(pkgLoc(P)))
  }

  openupRepo:(uri,uri) => result[(), termRepo].
  openupRepo(RU,CU) where CRU ^= resolveUri(CU,RU) => do{
    Repo .= openRepository(CRU);
    valis Repo
  }

  addSpec:(pkgSpec,termRepo) => termRepo.
  addSpec(Spec,R) where pkgSpec(Pkg,_,_) .= Spec => addSigToRepo(R,Pkg,(Spec::term)::string).

  importDecls((_,_,Decls))=>Decls.

  processPkgs:(cons[(pkg,cons[pkg])],termRepo,catalog,compilerOptions,reports) => result[reports,()].
  processPkgs(Pks,Repo,Cat,Opts,Rp) => do{
    Repp .= ref Repo;

    try{
      for (P,Imps) in Pks do{
--	logMsg("is $(P) ok? $(pkgOk(Repo,P))");
	if ~ {? (pkgOk(Repo,P) && I in Imps *> pkgOk(Repo,I)) ?} then{
	  logMsg("Compiling $(P)");
	  Rep1 <- processPkg(P,Repp!,Cat,Opts,Rp);
	  Repp := Rep1
	}
      };
      ignore flushRepo(Repp!);
    }catch (Erp) => do{
      logMsg("Errors $(Erp)");
      ignore flushRepo(Repp!);
      raise Erp
    }
  }
}
