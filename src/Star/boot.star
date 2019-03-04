star.boot{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.repo.
  import star.repo.boot.
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
      try{
        (Top,Args) <- handleCmdLineOpts(processOptions(_command_line(),[repoOption,wdOption],bootOptions("file:"++_repo(),"file:"++_cwd())));
        invokeMain(Top,Args)
      } catch (E) => do{ return logMsg(E) }
    }
  __boot() default => ().

  handleCmdLineOpts:(either[string,(bootOptions,list[string])])=>action[string,(string,list[string])].
  handleCmdLineOpts(either((bootOptions(RepoDir,Cwd),[Top,..Args]))) where
    CW ^= parseUri(Cwd) &&
    RU ^= parseUri(RepoDir) &&
    RD .= resolveUri(CW,RU) &&
    Repo .= bootRepo(RD) &&
    -- _ .= logMsg("Repo = \(Repo)") &&
    Pkg ^= parsePkgName(Top) => do{
      setupPkg(Repo,Pkg);
      return (Top,Args)
    }
  handleCmdLineOpts(other(E)) => err(E).

  setupPkg:(bootRepo,pkg) => action[string,()].
  setupPkg(Repo,Pkg) => do{
    importPkgs([Pkg],[],Repo);
    initialize(Pkg)
  }

  importPkgs:(list[pkg],list[pkg],bootRepo)=>action[string,()].
  importPkgs([],Ld,_) => (return ()).
  importPkgs([P,..L],Ld,R) where SubImp ^= importPkg(P,R,Ld) => importPkgs(SubImp++L,[P,..Ld],R).
  importPkgs(_,_,_) default => err("Could not load \(_command_line())").

  importPkg:(pkg,bootRepo,list[pkg])=>option[list[pkg]].
  importPkg(P,_,Ld) where contains(P,Ld) => some([]).
  importPkg(P,R,Ld) where
    Code ^= loadFromRepo(R,P) &&
    Imps .= _install_pkg(Code) => some(Imps//(((Pk,V))=>pkg(Pk,V::version))).
  importPkg(_,_,_) default => none.

  loadFromRepo:all r ~~ repo[r] |: (r,pkg) => option[string].
  loadFromRepo(Repo,Pkg) where
    U ^= hasResource(Repo,Pkg,"code") &&
    Uri ^= parseUri(U) => getResource(resolveUri(repoRoot(Repo),Uri)).
  loadFromRepo(_,_) default => none.

  initialize:(pkg) => action[string,()].
  initialize(pkg(P,_)) => do{
    Pred = P++"@init";
    if _definedLbl(Pred,0) then {
      return _callLbl(Pred,0,[])
    } else
      throw "No init for \(P)"
  }

  invokeMain:(string,list[string]) => action[string,()].
  invokeMain(Top,Args) => do {
    Pred = Top++"#_main";
    if _definedLbl(Pred,1) then {
      return _callLbl(Pred,1,[Args])
    }
    else
      throw "No main program: \(Top)".
  }
}
