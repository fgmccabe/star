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
    WI=^parseUri("file:"++_cwd());
    RI=^parseUri("file:"++_repo());
    try{
      handleCmds(processOptions(Args,[wdOption,
	    stdinOption,
	    traceAstOption,
	    traceMacroOption],
	  defltOptions(WI,RI)
	));
    } catch {
      Msg => logMsg("Fatal issue: #(Msg)")
    };
    valis ()
  }.

  handleCmds:((compilerOptions,cons[string]))=>() throws string.
  handleCmds((Opts,Args)) => valof{
    if CatUri ^= parseUri("catalog") && CatU ^= resolveUri(Opts.cwd,CatUri) &&
	Cat ^= loadCatalog(CatU) then{
	  for P in Args do{
	    resetErrors();
	    processPkg(extractPkgSpec(P),Cat,Opts)
	  }
	}
    else{
      throw "could not access catalog"
    };
    valis ()
  }

  extractPkgSpec(P) where Lc ^= strFind(P,":",0) => pkg(P[0:Lc],P[Lc+1:size(P)]::version).
  extractPkgSpec(P) default => pkg(P,.defltVersion).

  processPkg:(pkg,catalog,compilerOptions) => ()
  processPkg(P,Cat,Opts) => valof{
    logMsg("Macro processing $(P)");
    if (SrcUri,CPkg) ^= resolveInCatalog(Cat,pkgName(P)) then{
      Ast = ^parseSrc(SrcUri,CPkg);
      if traceAst! then{
	logMsg("Ast of $(P) is $(Ast)")
      };
      M = macroPkg(Ast);
      valis logMsg("Macrod package is $(M)");
    }
    else{
      reportError("cannot locate source of $(P)",some(pkgLoc(P)));
      valis ()
    }
  }
}
