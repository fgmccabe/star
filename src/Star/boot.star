star.boot{
  import star.
  import star.cmdOpts.
  import star.pkg.
  import star.repo.
  import star.repo.boot.
  import star.resources.
  import star.uri.

  bootOptions ::= bootOptions(string,string).

  repoOption:cmdOption[bootOptions].
  repoOption = cmdOption{
    shortForm = "-r".
    alternatives = [].
    usage = "-r dir -- directory of code repository".
    validator = some(isDir).
    setOption(R,bootOptions(_,W)) => bootOptions(R,W).
  }

  wdOption:cmdOption[bootOptions].
  wdOption = cmdOption{
    shortForm = "-w".
    alternatives = [].
    usage = "-w dir -- override working directory".
    validator = some(isDir).
    setOption(W,bootOptions(R,_)) => bootOptions(R,W).
  }

  public __boot:()=>().
  __boot() => valof{
    try{
--	logMsg("starting boot");
      Opts .= processOptions(_command_line(),
	[repoOption,wdOption],bootOptions("file:"++_repo(),"file:"++_cwd()));
      (Top,Args) <- handleCmdLineOpts(Opts);
--	logMsg("Args $(Args)");
      perform invokeMain(Top,Args)
    } catch (E) => logMsg(E)
  }

  handleCmdLineOpts:(either[string,(bootOptions,cons[string])])=>
    result[string,(string,cons[string])].
  handleCmdLineOpts(either((bootOptions(RepoDir,Cwd),[Top,..Args]))) where
      CW ^= parseUri(Cwd) &&
      RU ^= parseUri(RepoDir) &&
      RD ^= resolveUri(CW,RU) &&
      Pkg ^= parsePkgName(Top) => do{
	Repo .= btRepo(RD);
	setupPkg(Repo,Pkg);
	valis (Top,Args)
      }.
  handleCmdLineOpts(other(E)) => bad(E).

  setupPkg:(repository,pkg) => result[string,()].
  setupPkg(Repo,Pkg) => do{
    importPkgs([Pkg],[],Repo)
  }

  importPkgs:(cons[pkg],cons[pkg],repository)=>result[string,()].
  importPkgs([],Ld,_) => do {valis ()}.
  importPkgs([P,..L],Ld,R) where SubImp ^= importPkg(P,R,Ld) => importPkgs(SubImp++L,[P,..Ld],R).
  importPkgs(_,_,_) default => err("Could not load $(_command_line())").

  importPkg:(pkg,repository,cons[pkg])=>option[cons[pkg]].
  importPkg(P,_,Ld) where P .<. Ld => some([]).
  importPkg(P,R,Ld) where
    Code ^= R.hasCode(P) &&
    Imps .= _install_pkg(Code) => some(Imps//(((Pk,V))=>pkg(Pk,V::version))).
  importPkg(_,_,_) default => .none.

  invokeMain:(string,cons[string]) => result[string,()].
  invokeMain(Top,Args) => do {
    Pred .= Top++"#_main";
    if _definedLbl(Pred,1) then {
      valis _callLbl(Pred,1,[Args])
    }
    else
    raise "No _main program: $(Top)".
  }
}
