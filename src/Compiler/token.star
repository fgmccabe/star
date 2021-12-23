star.compiler.token{
  import star.
  import star.compiler.location.

  public token ::= tok(locn,tk) | endTok(locn).

  public tk ::= idQTok(string)
    | idTok(string)
    | lftTok(string)
    | rgtTok(string)
    | intTok(integer)
    | bigTok(string)
    | fltTok(float)
    | chrTok(char)
    | strTok(cons[stringSegment]).

  public stringSegment ::= segment(locn,string)
    | interpolate(locn,cons[token],string)
    | evaluate(locn,cons[token]).

  public implementation display[tk] => {
    disp(idQTok(Id)) => "''#(Id)''".
    disp(idTok(Id)) => "'#(Id)'".
    disp(intTok(Ix)) => disp(Ix).
    disp(bigTok(Bx)) => disp(Bx).
    disp(fltTok(Dx)) => disp(Dx).
    disp(chrTok(Ch)) => disp(Ch).
    disp(strTok(S)) => "\"#(dispSegments(S)*)\"".
    disp(lftTok(Id)) => "<#(Id)".
    disp(rgtTok(Id)) => "#(Id)>".
  }

  dispSegments:(cons[stringSegment]) => cons[string].
  dispSegments(Segs) => (Segs//disp).

  public implementation display[stringSegment] => {
    disp(segment(_,S)) => S.
    disp(interpolate(_,S,"")) => "\$($(S))".
    disp(interpolate(_,S,F)) => "\$($(S)):#(F);".
    disp(evaluate(_,S)) => "\#($(S))".
  }

  implementation equality[stringSegment] => {
    segment(_,S1) == segment(_,S2) => S1==S2.
    interpolate(_,S1,F1) == interpolate(_,S2,F2) => S1==S2 && F1==F2.
    evaluate(_,S1) == evaluate(_,S2) => S1==S2.
    _ == _ default => .false.
  }

  public implementation equality[tk] => {
    idQTok(Id1) == idQTok(Id2) => Id1==Id2.
    idTok(Id1) == idTok(Id2) => Id1==Id2.
    intTok(Ix1) == intTok(Ix2) => Ix1==Ix2.
    bigTok(Ix1) == bigTok(Ix2) => Ix1==Ix2.
    fltTok(Dx1) == fltTok(Dx2) => Dx1==Dx2.
    chrTok(C1) == chrTok(C2) => C1==C2.
    strTok(S1) == strTok(S2) => S1==S2.
    lftTok(S1) == lftTok(S2) => S1==S2.
    rgtTok(S1) == rgtTok(S2) => S1==S2.
    _ == _ default => .false.
  }

  public implementation equality[token] => {
    tok(_,T1)==tok(_,T2) => T1==T2.
    endTok(_)==endTok(_) => .true.
    _ == _ default => .false.
  }

  public implementation display[token] => {
    disp(tok(Lc,Tk)) => "$(Tk)@$(Lc)".
    disp(endTok(Lc)) => "end of stream@$(Lc)".
  }

  public implementation hasLoc[token] => {
    locOf(tok(Lc,_)) => Lc.
  }
}
