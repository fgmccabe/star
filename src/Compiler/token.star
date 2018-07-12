star.compiler.token{
  import star.
  import star.compiler.location.

  public token ::= tok(locn,tk).

  public tk ::= idQTok(string)
              | idTok(string)
              | intTok(integer)
              | fltTok(float)
              | strTok(string).

  public implementation display[tk] => {.
    disp(idQTok(Id)) => ssSeq([ss("''"),ss(Id),ss("''")]).
    disp(idTok(Id)) => ssSeq([ss("'"),ss(Id),ss("'")]).
    disp(intTok(Ix)) => disp(Ix).
    disp(fltTok(Dx)) => disp(Dx).
    disp(strTok(S)) => ssSeq([ss("\""),ss(S),ss("\"")]).
  .}

  public implementation equality[tk] => {.
    idQTok(Id1) == idQTok(Id2) => Id1==Id2.
    idTok(Id1) == idTok(Id2) => Id1==Id2.
    intTok(Ix1) == intTok(Ix2) => Ix1==Ix2.
    fltTok(Dx1) == fltTok(Dx2) => Dx1==Dx2.
    strTok(S1) == strTok(S2) => S1==S2.
    _ == _ default => false.
  .}

  public implementation display[token] => {.
    disp(tok(Lc,Tk)) => ssSeq([disp(Tk),ss("@"),disp(Lc)]).
  .}
}
