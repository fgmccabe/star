star.compiler.errors{
  import star.
  import star.compiler.location.

  public reportMsg ::= errorMsg(option[locn],string)
                       | warnMsg(option[locn],string).

  public reports ::= reports(cons[reportMsg]).

  public implementation display[reports] => {
    disp(reports(M)) => "#(interleave(M//disp,"\n")*)\n$(countErrors(M)) errors\n$(countWarnings(M)) warnings".
  }

  public implementation display[reportMsg] => {
    disp(errorMsg(.none,Msg)) => "error #(Msg)".
    disp(errorMsg(some(Lc),Msg)) => "error #(Msg) at $(Lc)".
    disp(warnMsg(.none,Msg)) => "warning #(Msg)".
    disp(warnMsg(some(Lc),Msg)) => "warning #(Msg) at $(Lc)".
  }

  public errorFree:(reports)=>boolean.
  errorFree(reports(Ms)) => countErrors(Ms)==0.

  public countErrors:(cons[reportMsg])=>integer.
  countErrors(Ms) => foldLeft(countError,0,Ms).

  countError(errorMsg(_,_),Ix)=>Ix+1.
  countError(_,Ix) default => Ix.

  public countWarnings:(cons[reportMsg])=>integer.
  countWarnings(Ms) => foldLeft(countWarning,0,Ms).

  countWarning(warnMsg(_,_),Ix)=>Ix+1.
  countWarning(_,Ix) default => Ix.

  public reportError:(reports,string,option[locn]) => reports.
  reportError(reports(Ms),Msg,Lc) => reports([errorMsg(Lc,Msg),..Ms]).

  public reportWarning:(reports,string,option[locn]) => reports.
  reportWarning(reports(Ms),Msg,Lc) => reports([warnMsg(Lc,Msg),..Ms]).
}
