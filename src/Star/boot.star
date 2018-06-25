star.boot{
  import star.
  import star.cmdOpts.
  import star.either.
  import star.repo.
  import star.repo.file.
  import star.uri.

  bootOptions ::= bootOptions{ repo:string. wd:string. }.

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
  __boot() where WD.=cwd() =>
    handleCmdLineOpts(processOptions(_command_line(),[repoOption,wdOption],bootOptions(WD,WD))).

  handleCmdLineOpts:(either[(bootOptions,list[string]),string])=>().
  handleCmdLineOpts(either((Opts,[Top,..Args]))) where
    RD .= resolveUri(parseUri(cwd()),parseUri(Opts.repo)) &&
    Repo .= (coreRepo,openRepository(RD)) &&
    _ .= importPkgs([parsePkgName(Top)],[],_,Repo) =>
    invokeMain(Top,Args).

  importPkgs:all r ~~ repo[r] |: (list[pkg],list[pkg],list[pkg],r)=>().
  importPkgs([],Ld,Ld,_)=>().
  importPkgs([P,..L],Ld,Ldx,R) where
    _ .= _logmsg("importing \(P)") &&
    Imps .= importPkg(P,R,Ld) &&
    _ .= importPkgs(Imps,[P,..Ld],Ldx,R) => initialize(P).

  importPkg:all r ~~ repo[r] |: (pkg,r,list[pkg])=>list[pkg]).
  importPkg(P,_,Ld) where P in Ld => [].
  importPkg(P,R,Ld) => where
    Code .= loadFromRepo(R,P,"code") &&
    Imps .= _install_pkg(Code) => Imps//(((Pk,V))=>pkg(Pk,V::version)).

  initialize:(pkg) => ().
  initialize(pkg(P,_)) where
    Pred .= P+"@init" =>
    ( _definedLbl(Pred,0) ?
      _callLbl(Pred,0,[]) |
      logMsg("No init for \(P)")).

  invokeMain:(string,list[string]) => ().
  invokeMain(Top,Args) where
    Pred .= Top+"@_main" =>
    ( _definedLbl(Pred,1) ?
      logMsg("Starting ..."),
      _callLbl(Pred,1,[Args]) |
      logMsg("No main program: \(Top)") ).

}
