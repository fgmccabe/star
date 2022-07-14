star.compiler.errors{
  import star.
  import star.compiler.location.

  reportMsg ::= errorMsg(option[locn],string)
    | warnMsg(option[locn],string).

  implementation display[reportMsg] => {
    disp(errorMsg(.none,Msg)) => "error #(Msg)".
    disp(errorMsg(some(Lc),Msg)) => "error #(Msg) at $(Lc)".
    disp(warnMsg(.none,Msg)) => "warning #(Msg)".
    disp(warnMsg(some(Lc),Msg)) => "warning #(Msg) at $(Lc)".
  }

  public displayErrorsAndWarnings() => "#(interleave(reports!//disp,"\n")*)\n$(countErrors()) errors\n$(countWarnings()) warnings".

  reports:ref cons[reportMsg].
  reports = ref [].

  public resetErrors() => valof{
    reports := [];
    valis ()
  }

  public errorFree:()=>boolean.
  errorFree() => countErrors()==0.

  public countErrors:()=>integer.
  countErrors() => foldLeft(countError,0,reports!).

  countError(errorMsg(_,_),Ix)=>Ix+1.
  countError(_,Ix) default => Ix.

  public countWarnings:()=>integer.
  countWarnings() => foldLeft(countWarning,0,reports!).

  countWarning(warnMsg(_,_),Ix)=>Ix+1.
  countWarning(_,Ix) default => Ix.

  public reportError:(string,option[locn]) => ().
  reportError(Msg,Lc) => valof{
    reports := [errorMsg(Lc,Msg),..reports!];
    valis ()
  }

  public reportWarning:(string,option[locn]) => ().
  reportWarning(Msg,Lc) => valof{
    reports := [warnMsg(Lc,Msg),..reports!];
    valis ()
  }
}
