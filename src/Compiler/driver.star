star.compiler{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.resources.
  import star.uri.

  import star.repo.

  import star.compiler.ast.
  import star.compiler.catalog.
  import star.compiler.checker.
  import star.compiler.errors.
  import star.compiler.grapher.
  import star.compiler.parser.
  import star.compiler.location.

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
    handleCmdLineOpts(processOptions(Args,[repoOption,wdOption],compilerOptions(RI,WI))).

  handleCmdLineOpts:(either[string,(compilerOptions,list[string])])=>().
  handleCmdLineOpts(either((compilerOptions(RU,CU),Args))) where
      _ .= _logmsg("CU=$(CU), RU=$(RU)") &&
      CRU ^= resolveUri(CU,RU) &&
      Repo .= openRepository(CRU) &&
      _ .= _logmsg("opened repo") &&
      CatUri ^= parseUri("catalog") &&
      -- _ .= _logmsg("catalog = $(loadCatalog(resolveUri(CU,CatUri)))") &&
      CatU ^= resolveUri(CU,CatUri) &&
      Cat ^= loadCatalog(CatU) =>
    processPkgs(Args,Cat,Repo,CU).

  processPkgs:(list[string],catalog,fileRepo,uri) => ().
  processPkgs([],Cat,Repo,U) => ().
  processPkgs([Nm,..Rest],Cat,Repo,CWD) where
    _ .= processPkg(Nm,Cat) =>
      processPkgs(Rest,Cat,Repo,CWD).

  processPkg:(string,catalog) => ().
  processPkg(Nm,Cat) where
    (U,P) ^= resolveInCatalog(Cat,Nm) &&
    _ .= _logmsg("Parse $(P) in $(U)") &&
    (Ast,Rpt) .= parseSrc(U,P,reports([])) &&
    _ .= _logmsg("Src = $(Ast)") => ().

}
