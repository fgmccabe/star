star.compiler.errors{
  import star.
  import star.compiler.location.

  public reportMsg ::= errorMsg(locn,string)
                       | warnMsg(locn,string).

  public reports ::= reports(list[reportMsg]).

  public implementation display[reports] => {.
    disp(reports(M)) => ssSeq([ssSeq(M//disp),ss("\n"),disp(countErrors(M)),ss(" errors, "),
                               disp(countWarnings(M)),ss(" warnings.")]).
  .}

  public implementation display[reportMsg] => {.
    disp(errorMsg(Lc,Msg)) => ssSeq([ss("error "),ss(Msg),ss(" at "),disp(Lc)]).
    disp(warnMsg(Lc,Msg)) => ssSeq([ss("warning "),ss(Msg),ss(" at "),disp(Lc)]).
  .}

  public errorFree:(reports)=>boolean.
  errorFree(reports(Ms)) => countErrors(Ms)==0.

  public countErrors:(list[reportMsg])=>integer.
  countErrors(Ms) => foldLeft(countError,0,Ms).

  countError(Ix,errorMsg(_,_))=>Ix+1.
  countError(Ix,_) default => Ix.

  public countWarnings:(list[reportMsg])=>integer.
  countWarnings(Ms) => foldLeft(countWarning,0,Ms).

  countWarning(Ix,warnMsg(_,_))=>Ix+1.
  countWarning(Ix,_) default => Ix.

  public reportError:(reports,string,locn) => reports.
  reportError(reports(Ms),Msg,Lc) => reports([errorMsg(Lc,Msg),..Ms]).

  public reportWarning:(reports,string,locn) => reports.
  reportWarning(reports(Ms),Msg,Lc) => reports([warnMsg(Lc,Msg),..Ms]).
}
