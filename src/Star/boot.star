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
    RD .= resolveUri(parseUri(cwd()),parseUri(Opts.repo)),
    Repo .= (coreRepo,openRepository(RD)),
    importPkgs([parsePkgName(Top)],[],_,Repo),
    invokeMain(Top,Args).

  importPkgs:all r ~~ repository[r] |: (list[pkg],list[pkg],list[pkg],r){}.
  importPkgs([],Ld,Ld,_).
  importPkgs([P,..L],Ld,Ldx,R) :-
    _logmsg("importing \(P)"),
    importPkg(P,R,Ld,Imps),
    importPkgs(Imps,[P,..Ld],Ldx,R),
    initialize(P).

  importPkg:all r ~~ repository[r] |: (pkg,r,list[pkg],list[pkg]){}.
  importPkg(P,_,Ld,[]) :- P in Ld.
  importPkg(P,R,Ld,Imps//(((Pk,V))=>pkgUp(Pk,V))) :-
    loadFromRepo(R,P,"code",Code),
    Imps = _install_pkg(Code).

  pkgUp:(string,string) => pkg.
  pkgUp(P,"*") => pkg(P,defltVersion).
  pkgUp(P,V) => pkg(P,vers(V)).

  initialize:(pkg){}.
  initialize(pkg(P,_)) :-
    Pred = P+"@init",
    ( _defined(Pred,0) ?
      _call(Pred,0,[]) |
      logMsg("No init for \(P)")).

  invokeMain:(string,list[string]){}.
  invokeMain(Top,Args) :-
    Pred = Top+"@_main",
    ( _defined(Pred,1) ?
      logMsg("Starting ..."),
      _call(Pred,1,[Args]) |
      logMsg("No main program: \(Top)") ).

  private delayHandler:(list[(){}]){}. -- private, but known to the run-time
  delayHandler([]).
  delayHandler([H,..L]) :- H()!, delayHandler(L).
}
