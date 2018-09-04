star.boot{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.repo.
  import star.repo.file.
  import star.resources.
  import star.uri.

  bootOptions ::= bootOptions(string,string).

  repoOption:optionsProcessor[bootOptions].
  repoOption = {
    shortForm = "-r".
    alternatives = [].
    usage = "-r dir -- directory of code repository".
    validator = some(isDir).
    setOption(R,bootOptions(_,W)) => bootOptions(R,W).
  }

  wdOption:optionsProcessor[bootOptions].
  wdOption = {
    shortForm = "-w".
    alternatives = [].
    usage = "-w dir -- override working directory".
    validator = some(isDir).
    setOption(W,bootOptions(R,_)) => bootOptions(R,W).
  }

  public __boot:()=>().
  __boot() where _ .= _callLbl("star.boot@init",0,_list_nil(0)) =>
    handleCmdLineOpts(processOptions(_command_line(),[repoOption,wdOption],bootOptions("file:"++_repo(),"file:"++_cwd()))).

  handleCmdLineOpts:(either[(bootOptions,list[string]),string])=>().
  handleCmdLineOpts(either((bootOptions(RepoDir,Cwd),[Top,..Args]))) where
    CW ^= parseUri(Cwd) &&
    RU ^= parseUri(RepoDir) &&
    RD .= resolveUri(CW,RU) &&
    Repo .= openRepository(RD) &&
    -- _ .= logMsg("Manifest = \(Repo)") &&
    Pkg ^= parsePkgName(Top) &&
    _ .= importPkgs([Pkg],[],Repo) &&
    _ .= initialize(Pkg) =>
    invokeMain(Top,Args).

  importPkgs:(list[pkg],list[pkg],fileRepo)=>().
  importPkgs([],Ld,_)=>().
  importPkgs([P,..L],Ld,R) where
    _ .= _logmsg("importing \(P)") &&
    Imps .= importPkg(P,R,Ld) => importPkgs(Imps,[P,..Ld],R).

  importPkg:(pkg,fileRepo,list[pkg])=>list[pkg].
  importPkg(P,_,Ld) where contains(P,Ld) => [].
  importPkg(P,R,Ld) where
    Code ^= loadFromRepo(R,P,"code") &&
    Imps .= _install_pkg(Code) => Imps//(((Pk,V))=>pkg(Pk,V::version)).

  initialize:(pkg) => ().
  initialize(pkg(P,_)) where
    Pred .= P++"@init" =>
    ( _definedLbl(Pred,0) ?
      _callLbl(Pred,0,[]) |
      logMsg("No init for \(P)")).

  invokeMain:(string,list[string]) => ().
  invokeMain(Top,Args) where
    Pred .= Top++"@_main" =>
    ( _definedLbl(Pred,1) ?
      _callLbl(Pred,1,Args) |
      logMsg("No main program: \(Top)") ).

}
