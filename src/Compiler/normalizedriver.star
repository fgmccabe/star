star.compiler.normalize.driver{
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
	  traceDependencyOption,
	  traceAstOption,
	  traceMacroOption,
	  typeCheckOnlyOption,
	  traceCheckOption,
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
	      processPkg(extractPkgSpec(P),Repo,Cat,Opts,ErRp)
	    } catch (Er) => do{
	      logMsg("$(Er)");
	      valis _exit(9)
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

  processPkg:(pkg,termRepo,catalog,compilerOptions,reports) => result[reports,()].
  processPkg(P,Repo,Cat,Opts,Rp) => do{
    logMsg("Macro processing $(P)");
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
	C <- (checkPkg(Repo,CPkg,M,Opts,Rp)::result[reports,(pkgSpec,cons[canonDef],cons[decl])]);
	if traceCanon! then {
	  logMsg("type checked $(C)")
	};
      }
    }
    else
    raise reportError(Rp,"cannot locate source of $(P)",pkgLoc(P))
  }

  openupRepo:(uri,uri) => result[(), termRepo].
  openupRepo(RU,CU) where CRU ^= resolveUri(CU,RU) => do{
    Repo .= openRepository(CRU);
    valis Repo
  }
}
