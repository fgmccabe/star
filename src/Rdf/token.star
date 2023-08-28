rdf.token{
  import star.
  import star.location.
  import star.uri.

  public token ::= .tok(locn,tk) | .endTok(locn).

  public tk ::= .idQTok(string)
  | .idTok(string)
  | .intTok(integer)
  | .fltTok(float)
  | .chrTok(char)
  | .pncTok(string)
  | .uriTok(uri)
  | .strTok(cons[stringSegment]).

  public stringSegment ::= .segment(locn,string)
    | .interpolate(locn,cons[token],string)
  | .evaluate(locn,cons[token]).

  public implementation display[tk] => {
    disp(Tk) => case Tk in {
      .idQTok(Id) => "''#(Id)''".
      .idTok(Id) => "'#(Id)'".
      .intTok(Ix) => disp(Ix).
      .fltTok(Dx) => disp(Dx).
      .chrTok(Ch) => disp(Ch).
      .pncTok(S) => "%$(S)".
      .uriTok(U) => "<$(U)>".
      .strTok(S) => "\"#(dispSegments(S)*)\"".
    }
  }

  dispSegments:(cons[stringSegment]) => cons[string].
  dispSegments(Segs) => (Segs//disp).

  public implementation display[stringSegment] => {
    disp(Sg) => case Sg in {
      .segment(_,S) => S.
      .interpolate(_,S,"") => "\$($(S))".
      .interpolate(_,S,F) => "\$($(S)):#(F);".
      .evaluate(_,S) => "\#($(S))".
    }
  }

  implementation equality[stringSegment] => {
    .segment(_,S1) == .segment(_,S2) => S1==S2.
    .interpolate(_,S1,F1) == .interpolate(_,S2,F2) => S1==S2 && F1==F2.
    .evaluate(_,S1) == .evaluate(_,S2) => S1==S2.
    _ == _ default => .false.
  }

  public implementation equality[tk] => {
    Tk1 == Tk2 => case Tk1 in {
      .idQTok(Id1) => .idQTok(Id2).=Tk2 && Id1==Id2.
      .idTok(Id1) => .idTok(Id2).=Tk2 && Id1==Id2.
      .intTok(Ix1) => .intTok(Ix2).=Tk2 && Ix1==Ix2.
      .fltTok(Dx1) => .fltTok(Dx2).=Tk2 && Dx1==Dx2.
      .chrTok(C1) => .chrTok(C2).=Tk2 && C1==C2.
      .pncTok(C1) => .pncTok(C2).=Tk2 && C1==C2.
      .strTok(S1) => .strTok(S2).=Tk2 && S1==S2.
    }
  }

  public implementation equality[token] => {
    .tok(_,T1)==.tok(_,T2) => T1==T2.
    .endTok(_)==.endTok(_) => .true.
    _ == _ default => .false.
  }

  public implementation display[token] => {
    disp(.tok(Lc,Tk)) => "$(Tk)@$(locPos(Lc))".
    disp(.endTok(Lc)) => "end of stream@$(Lc)".
  }

  public implementation hasLoc[token] => {
    locOf(.tok(Lc,_)) => .some(Lc).
    locOf(.endTok(Lc)) => .some(Lc).
  }

  public bracket ::= .bkt(string,string,string,string).

  public isBracket:(string) => option[bracket].
  isBracket(Str) => case Str in {
    "[" => .some(.bkt("[","[]","]",",")).
    "]" => .some(.bkt("[","[]","]",",")).
    "[]" => .some(.bkt("[","[]","]",",")).
    "(" => .some(.bkt("(","()",")",",")).
    ")" => .some(.bkt("(","()",")",",")).
    "()" => .some(.bkt("(","()",")",",")).
    "{" => .some(.bkt("{","{}","}",".\n")).
    "}" => .some(.bkt("{","{}","}",".\n")).
    "{}" => .some(.bkt("{","{}","}",".\n")).
    _ default => .none.
  }
 }
