star.compiler{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.resources.
  import star.uri.

  import star.repo.
  import star.repo.file.

  import star.compiler.ast.
  import star.compiler.catalog.
  import star.compiler.checker.
  import star.compiler.errors.
  import star.compiler.grapher.
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.location.

  implementation all e,k ~~ coercion[either[e,k],action[e,k]] => {
    _coerce(either(X)) => done(X).
    _coerce(other(Y)) => err(Y)
  }

  compilerOptions ::= compilerOptions(uri,uri).

  repoOption:optionsProcessor[compilerOptions].
  repoOption = {
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = some(isDir).
    setOption(R,compilerOptions(_,W)) where RU ^= parseUri(R) && NR^=resolveUri(W,RU) =>
      compilerOptions(NR,W).
  }

  wdOption:optionsProcessor[compilerOptions].
  wdOption = {
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = some(isDir).
    setOption(W,compilerOptions(R,OW)) where RW ^= parseUri(W) && NW^=resolveUri(OW,RW)=>
      compilerOptions(R,NW).
  }

  public _main:(list[string])=>().
  _main(Args) where RI^=parseUri("file:"++_repo()) && WI^=parseUri("file:"++_cwd())=>
    valof handleCmds(processOptions(Args,[repoOption,wdOption],compilerOptions(RI,WI))).

  handleCmds:(either[string,(compilerOptions,list[string])])=>action[(),()].
  handleCmds(either((compilerOptions(RU,CU),Args))) => do{
    logMsg("CU=$(CU), RU=$(RU)");
    if CRU ^= resolveUri(CU,RU) then{
      Repo = openRepository(CRU);
      logMsg("opened repo");
      if CatUri ^= parseUri("catalog") && CatU ^= resolveUri(CU,CatUri) &&
	  Cat ^= loadCatalog(CatU) then{
	    for P in Args do{
	      ErRp = reports([]);	
	      try{
		Sorted <- makeGraph(extractPkgSpec(P),Repo,Cat,ErRp)::action[reports,list[(importSpec,list[importSpec])]];
		processPkgs(Sorted,Repo,Cat,ErRp);
		logMsg("done")
	      } catch (Er) => action{
		logMsg("$(Er)")
	      }
	    }
	  }
      else{
	logMsg("could not access catalog")
      }
    }
    else{
      logMsg("could not resolve catalog URI $(CU)")
    }
  }

  extractPkgSpec(P) where Lc .= _str_find(P,":",0) && Lc>0 => let{
    Pkg = pkg(_sub_str(P,0,Lc),_sub_str(P,Lc+1,_str_len(P))::version).
  } in pkgImp(pkgLoc(Pkg),priVate,Pkg).
  extractPkgSpec(P) default =>
    pkgImp(pkgLoc(pkg(P,defltVersion)),priVate,pkg(P,defltVersion)).

  processPkgs:(list[importSpec],fileRepo,catalog,reports) => action[reports,()].
  processPkgs(Pks,Repo,Cat,Rp) => do{
    for P in Pks do{
      logMsg("Process $(P)")
    }
  }
}
