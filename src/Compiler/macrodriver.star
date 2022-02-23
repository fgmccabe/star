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

  public _main:(cons[string])=>().
  _main(Args) => valof{
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

  extractPkgSpec(P) where Lc ^= strFind(P,":",0) => pkg(P[0:Lc],P[Lc+1:size(P)]::version).
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
      if traceAst! then{
	logMsg("Ast of $(P) is $(Ast)")
      };
      M <- macroPkg(Ast,Rp);
      logMsg("Macrod package is $(M)");
    }
    else
    raise reportError(Rp,"cannot locate source of $(P)",pkgLoc(P))
  }
}
