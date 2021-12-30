star.compiler.macro.driver{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.file.
  import star.resources.
  import star.uri.

  import star.compiler.ast.
  import star.compiler.catalog.
  import star.compiler.errors.
  import star.compiler.macro.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.parser.
  import star.compiler.location.

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
	typeCheckOnly=Opts.typeCheckOnly.
	doStdin=Opts.doStdin
      }.
  }

  public _main:(cons[string])=>().
  _main(Args) => valof action{
    WI^=parseUri("file:"++_cwd());
    RI^=parseUri("file:"++_repo());
    handleCmds(processOptions(Args,[wdOption,
	  stdinOption,
	  traceAstOption,
	  traceMacroOption],
	defltOptions(WI,RI)
      ))
  }.

  handleCmds:(either[string,(compilerOptions,cons[string])])=>result[(),()].
  handleCmds(either((Opts,Args))) => do{
    if CatUri ^= parseUri("catalog") && CatU ^= resolveUri(Opts.cwd,CatUri) &&
	Cat ^= loadCatalog(CatU) then{
	  for P in Args do{
	    ErRp .= reports([]);	
	    try{
	      processPkg(extractPkgSpec(P),Cat,Opts,ErRp)
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

  extractPkgSpec(P) where Lc .= strFind(P,":",0) && Lc>0 => pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  extractPkgSpec(P) default => pkg(P,.defltVersion).

  implementation all e,k ~~ coercion[(option[k],e),result[e,k]] => {
    _coerce((.none,R)) => some(err(R)).
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
    }
    else
    raise reportError(Rp,"cannot locate source of $(P)",pkgLoc(P))
  }
}
