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
  __boot() where _ .= _callLbl("star.boot@init",0,_list_nil(0)) &&
    (Top,Args) ^= handleCmdLineOpts(processOptions(_command_line(),[repoOption,wdOption],bootOptions("file:"++_repo(),"file:"++_cwd()))) =>
    invokeMain(Top,Args).
  __boot() default => ().

  handleCmdLineOpts:(either[string,(bootOptions,list[string])])=>either[string,(string,list[string])].
  handleCmdLineOpts(either((bootOptions(RepoDir,Cwd),[Top,..Args]))) where
    CW ^= parseUri(Cwd) &&
    RU ^= parseUri(RepoDir) &&
    RD .= resolveUri(CW,RU) &&
    Repo .= openRepository(RD) &&
    Pkg ^= parsePkgName(Top) => valof do{
      setupPkg(Repo,Pkg);
      ^^ (Top,Args)
    }
  handleCmdLineOpts(other(E)) => other(E).

  setupPkg:(fileRepo,pkg) => action[string,()].
  setupPkg(Repo,Pkg) => do{
    importPkgs([Pkg],[],Repo);
    initialize(Pkg);
  }

  importPkgs:(list[pkg],list[pkg],fileRepo)=>either[string,()].
  importPkgs([],Ld,_)=> either(()).
  importPkgs([P,..L],Ld,R) where SubImp ^= importPkg(P,R,Ld) => importPkgs(SubImp++L,[P,..Ld],R).
  importPkgs(_,_,_) default => other("Could not load \(_command_line())").

  importPkg:(pkg,fileRepo,list[pkg])=>option[list[pkg]].
  importPkg(P,_,Ld) where contains(P,Ld) => some([]).
  importPkg(P,R,Ld) where
    Code ^= loadFromRepo(R,P,"code") &&
    Imps .= _install_pkg(Code) => some(Imps//(((Pk,V))=>pkg(Pk,V::version))).
  importPkg(_,_,_) default => none.

  initialize:(pkg) => ().
  initialize(pkg(P,_)) where
    Pred .= P++"@init" =>
    ( _definedLbl(Pred,0) ?
      _callLbl(Pred,0,[]) ||
      logMsg("No init for \(P)")).

  invokeMain:(string,list[string]) => ().
  invokeMain(Top,Args) where
    Pred .= Top++"#_main" =>
    ( _definedLbl(Pred,1) ?
      _callLbl(Pred,1,[Args]) ||
      logMsg("No main program: \(Top)") ).

}
