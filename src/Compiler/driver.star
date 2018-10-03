star.compiler{
  import star.
  import star.cmdOpts.
  import star.resources.
  import star.uri.

  import star.repo.file.

  import star.compiler.catalog.
  import star.compiler.lexer.
  import star.compiler.location.


  compilerOptions ::= compilerOptions(string,string).

  repoOption:optionsProcessor[compilerOptions].
  repoOption = {
    shortForm = "-R".
    alternatives = [].
    usage = "-R dir -- directory of repository".
    validator = some(isDir).
    setOption(R,compilerOptions(_,W)) => compilerOptions(R,W).
  }

  wdOption:optionsProcessor[compilerOptions].
  wdOption = {
    shortForm = "-W".
    alternatives = [].
    usage = "-W dir -- working directory".
    validator = some(isDir).
    setOption(W,compilerOptions(R,_)) => compilerOptions(R,W).
  }

  public _main:(list[string])=>().
  _main(Args) =>
    handleCmdLineOpts(processOptions(Args,[repoOption,wdOption],compilerOptions("file:"++_repo(),"file:"++_cwd()))).

  handleCmdLineOpts:(either[(compilerOptions,list[string]),string])=>().
  handleCmdLineOpts(either((compilerOptions(RepoDir,Cwd),Args))) where
    CW ^= parseUri(Cwd) &&
    RU ^= parseUri(RepoDir) &&
    RD .= resolveUri(CW,RU) &&
    Repo .= openRepository(RD) &&
    CatUri ^= parseUri("catalog") &&
    Cat ^= loadCatalog(resolveUri(CW,CatUri)) =>
    processPkgs(Args,Cat,Repo,CW).

  processPkgs:(list[string],catalog,fileRepo,uri) => ().
  processPkgs(Names,Cat,Repo,U) => ().



}
