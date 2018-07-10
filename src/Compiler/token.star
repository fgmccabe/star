star.compiler.token{
  import star.
  import star.compiler.location.

  public token ::= tok(locn,tk).

  public tk ::= idQTok(string)
              | idTok(string)
              | intTok(integer)
              | fltTok(float)
              | strTok(list[stringSeg]).
  public stringSeg ::= segment(string) | interpolate(string,string,locn).

  public implementation display[tk] => {.
    disp(idQTok(Id)) => ssSeq([ss("'"),ss(Id),ss("'")]).
    disp(idTok(Id)) => ss(Id).
    disp(intTok(Ix)) => disp(Ix).
    disp(fltTok(Dx)) => disp(Dx).
    disp(strTok(S)) => ssSeq([ss("\""),ssSeq(S//disp),ss("\"")]).
  .}

  implementation display[stringSeg] => {.
    disp(segment(S)) => ss(S).
    disp(interpolate(S,F,_)) => ssSeq([ss("\\("),ss(S),ss("):"),ss(F),ss(";")]).
  .}

  public implementation display[token] => {.
    disp(tok(Lc,Tk)) => ssSeq([disp(Tk),ss("@"),disp(Lc)]).
  .}
}
