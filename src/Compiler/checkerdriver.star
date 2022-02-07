star.compiler.checker.driver{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.resources.
  import star.uri.

  import star.repo.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.catalog.
  import star.compiler.checker.
  import star.compiler.core.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.grapher.
  import star.compiler.impawt.
  import star.compiler.macro.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.parser.
  import star.compiler.location.
  import star.compiler.term.repo.
  import star.compiler.types.


  repoOption:cmdOption[compilerOptions].
  repoOption = cmdOption{
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = some(isDir).
    setOption(R,Opts) where RU ^= parseUri(R) && NR^=resolveUri(Opts.cwd,RU) =>
      compilerOptions{repo=NR.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      }.
  }

  wdOption:cmdOption[compilerOptions].
  wdOption = cmdOption{
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = some(isDir).
    setOption(W,Opts) where RW ^= parseUri(W) && NW^=resolveUri(Opts.cwd,RW)=>
      compilerOptions{repo=Opts.repo.
	cwd=NW.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	macroOnly=Opts.macroOnly.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      }.
  }

  stdinOption:cmdOption[compilerOptions].
  stdinOption = cmdOption{
    shortForm = "".
    alternatives = ["--stdin"].
    usage = "--stdin -- compile standard input".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	macroOnly=Opts.macroOnly.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=.true.
      }.
  }

  traceAstOption:cmdOption[compilerOptions].
  traceAstOption = cmdOption{
    shortForm = "-dA".
    alternatives = [].
    usage = "-dA -- show ast".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = .true.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	macroOnly=Opts.macroOnly.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      }.
  }

  traceMacroOption:cmdOption[compilerOptions].
  traceMacroOption = cmdOption{
    shortForm = "-dM".
    alternatives = [].
    usage = "-dM -- show macro".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=.true.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	macroOnly=Opts.macroOnly.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      }.
  }

  macroOnlyOption:cmdOption[compilerOptions].
  macroOnlyOption = cmdOption{
    shortForm = "-m".
    alternatives = ["--macro-only"].
    usage = "-m -- only do macros".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	macroOnly=.true.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin}.
  }

  typeCheckOnlyOption:cmdOption[compilerOptions].
  typeCheckOnlyOption = cmdOption{
    shortForm = "-c".
    alternatives = ["--compile-only"].
    usage = "-c -- type check".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=Opts.showCanon.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	macroOnly=Opts.macroOnly.
	typeCheckOnly=.true.
	doStdin=Opts.doStdin}.
  }

  traceCheckOption:cmdOption[compilerOptions].
  traceCheckOption = cmdOption{
    shortForm = "-dt".
    alternatives = [].
    usage = "-dt -- show type checkedcode".
    validator = .none.
    setOption(_,Opts) =>
      compilerOptions{repo=Opts.repo.
	cwd=Opts.cwd.
	graph=Opts.graph.
	optimization=Opts.optimization.
	showAst = Opts.showAst.
	showMacro=Opts.showMacro.
	showCanon=.true.
	showCore=Opts.showCore.
	showCode=Opts.showCode.
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin}.
  }

  public _main:(cons[string])=>().
  _main(Args) => valof{
    WI^=parseUri("file:"++_cwd());
    RI^=parseUri("file:"++_repo());
    handleCmds(processOptions(Args,[wdOption,
	  stdinOption,
	  traceAstOption,
	  traceMacroOption,
	  traceCheckerOption,
	  macroOnlyOption,
	  typeCheckOnlyOption],
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
	      Sorted <- makeGraph(extractPkgSpec(P),Repo,Cat,ErRp)
	      ::action[reports,cons[(importSpec,cons[importSpec])]];
	      if Grph ^= Opts.graph then {
		ignore(()=>putResource(Grph,makeDotGraph(P,Sorted)))
	      };
		
	      processPkgs(Sorted,Repo,Cat,Opts,ErRp)
	    } catch (Er) => action{
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

  extractPkgSpec(P) where Lc .= strFind(P,":",0) && Lc>0 => pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  extractPkgSpec(P) default => pkg(P,.defltVersion).

  implementation all e,k ~~ coercion[(option[k],e),result[e,k]] => {
    _coerce((.none,R)) => some(bad(R)).
    _coerce((some(A),_)) => some(ok(A)).
  }

  processPkg:(pkg,catalog,compilerOptions,reports) => result[reports,()].
  processPkg(P,Cat,Opts,Rp) => do{
    logMsg("Macro processing $(P)");
    if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(P)) then{
      Ast <- parseSrc(SrcUri,CPkg,Rp)::result[reports,ast];
      if Opts.showAst then{
	logMsg("Ast of $(P) is $(Ast)")
      };
      M <- macroPkg(Ast,Rp);
      logMsg("Macrod package is $(M)");

      if ~ Opts.macroOnly then{
	C <- checkPkg(RCPkg);
	if Opts.showCanon then {
	  logMsg("type checked $(PkgFun)")
	};
      }
    }
    else
    raise reportError(Rp,"cannot locate source of $(P)",pkgLoc(P))
  }

  openupRepo:(uri,uri) => action[(), termRepo].
  openupRepo(RU,CU) where CRU ^= resolveUri(CU,RU) => do{
    Repo .= openRepository(CRU);
    valis Repo
  }

  private ignore(F) => action{
    _ .= F();
    valis ()
  }
}
