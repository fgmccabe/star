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
    valof do{
      (Top,Args) <- handleCmdLineOpts(processOptions(_command_line(),[repoOption,wdOption],bootOptions("file:"++_repo(),"file:"++_cwd())));
      invokeMain(Top,Args)
      -- >>> ((E) => do{ ^^ logMsg(E) })
    }
  __boot() default => ().

  handleCmdLineOpts:(either[string,(bootOptions,list[string])])=>action[string,(string,list[string])].
  handleCmdLineOpts(either((bootOptions(RepoDir,Cwd),[Top,..Args]))) where
    CW ^= parseUri(Cwd) &&
    RU ^= parseUri(RepoDir) &&
    RD .= resolveUri(CW,RU) &&
    Repo .= openRepository(RD) &&
    -- _ .= logMsg("Repo = \(Repo)") &&
    Pkg ^= parsePkgName(Top) => do{
      setupPkg(Repo,Pkg);
      return (Top,Args)
    }
  handleCmdLineOpts(other(E)) => err(E).

  setupPkg:(fileRepo,pkg) => action[string,()].
  setupPkg(Repo,Pkg) => do{
    importPkgs([Pkg],[],Repo);
    initialize(Pkg)
  }

  importPkgs:(list[pkg],list[pkg],fileRepo)=>action[string,()].
  importPkgs([],Ld,_) => (return ()).
  importPkgs([P,..L],Ld,R) where SubImp ^= importPkg(P,R,Ld) => importPkgs(SubImp++L,[P,..Ld],R).
  importPkgs(_,_,_) default => err("Could not load \(_command_line())").

  importPkg:(pkg,fileRepo,list[pkg])=>option[list[pkg]].
  importPkg(P,_,Ld) where contains(P,Ld) => some([]).
  importPkg(P,R,Ld) where
    -- _ .= logMsg("loading \(P)") &&
    Code ^= loadFromRepo(R,P,"code") &&
    --  _ .= logMsg("found \(P)") &&
    Imps .= _install_pkg(Code) => some(Imps//(((Pk,V))=>pkg(Pk,V::version))).
  importPkg(_,_,_) default => none.

  initialize:(pkg) => action[string,()].
  initialize(pkg(P,_)) where
    Pred .= P++"@init" =>
    ( _definedLbl(Pred,0) ?
      (return _callLbl(Pred,0,[])) ||
      _raise("No init for \(P)")).

  invokeMain:(string,list[string]) => action[string,()].
  invokeMain(Top,Args) where
    Pred .= Top++"#_main" =>
    ( _definedLbl(Pred,1) ?
      return _callLbl(Pred,1,[Args]) ||
      err("No main program: \(Top)") ).

}
