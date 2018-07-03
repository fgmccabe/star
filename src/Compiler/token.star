star.compiler.token{
  import star.
  import star.compiler.location.

  public token ::= tok(locn,tk).

  public tk ::= lpTok(integer)
              | rpTok(integer)
              | idQTok(string)
              | idTok(string)
              | intTok(integer)
              | fltTok(float)
              | strTok(list[stringSeg])
              | termTok.
  public stringSeg ::= segment(string) | interpolate(string,string,locn).

  public implementation display[tk] => {.
    disp(lpTok(Ch)) => ssSeq([ss("<<"),sc(Ch)]).
    disp(rpTok(Ch)) => ssSeq([ss(">>"),sc(Ch)]).
    disp(idQTok(Id)) => ssSeq([ss("'"),ss(Id),ss("'")]).
    disp(idTok(Id)) => ss(Id).
    disp(intTok(Ix)) => disp(Ix).
    disp(fltTok(Dx)) => disp(Dx).
    disp(strTok(S)) => ssSeq([ss("\""),ssSeq(S//disp),ss("\"")]).
    disp(termTok) => ss("<term>").
  .}

  implementation display[stringSeg] => {.
    disp(segment(S)) => ss(S).
    disp(interpolate(S,F,_)) => ssSeq([ss("\\("),ss(S),ss("):"),ss(F),ss(";")]).
  .}

  public implementation display[token] => {.
    disp(tok(Lc,Tk)) => ssSeq([disp(Tk),ss("@"),disp(Lc)]).
  .}
}
