star.compiler.checker.driver{
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
  import star.compiler.term.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.macro.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.parser.
  import star.compiler.location.
  import star.compiler.term.repo.
  import star.compiler.types.


  public _main:(cons[string])=>().
  _main(Args) => valof{
    WI=^parseUri("file:"++_cwd());
    RI=^parseUri("file:"++_repo());
    try{
      valis handleCmds(processOptions(Args,[wdOption,
	    stdinOption,
	    repoOption,
	    traceDependencyOption,
	    traceAstOption,
	    traceMacroOption,
	    showCheckOption,
	    traceCheckOption,
	    macroOnlyOption],
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
    
    if CatUri ^= parseUri("catalog") && CatU ^= resolveUri(Opts.cwd,CatUri) &&
	Cat ^= loadCatalog(CatU) then{
	  for P in Args do{
	    resetErrors();
	    if Pk ^= P:?pkg then{
	      processPkg(Pk,Repo,Cat)
	    } else{
	      logMsg("cannot parse $(P) as a package name")
	    }
	  }
	}
    else{
      logMsg("could not access catalog")
    };
    valis ()
  }

  extractPkgSpec(P) where Lc ^= strFind(P,":",0) => pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  extractPkgSpec(P) default => pkg(P,.defltVersion).

  processPkg:(pkg,termRepo,catalog) => ().
  processPkg(P,Repo,Cat) => valof{
    logMsg("Processing $(P)");
    if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(P)) then{
      Ast = ^parseSrc(SrcUri,CPkg);
      if traceAst! then{
	logMsg("Ast of $(P) is $(Ast)")
      };
      M = macroPkg(Ast);
      if traceMacro! then{
	logMsg("Macroed package $(M)")
      };

      if ~ macroOnly! then{
	C = checkPkg(Repo,CPkg,M);
	if showCanon! then {
	  logMsg("type checked $(fst(C)), $(snd(C))")
	};
      };
      valis ()
    }
    else{
      reportError("cannot locate source of $(P)",some(pkgLoc(P)));
      valis ()
    }
  }

  openupRepo:(uri,uri) => termRepo.
  openupRepo(RU,CU) where CRU ^= resolveUri(CU,RU) => valof{
    Repo = openRepository(CRU);
    valis Repo
  }
}
