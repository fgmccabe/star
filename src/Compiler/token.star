star.compiler.token{
  import star.
  import star.compiler.location.
  import star.compiler.operators.

  public token ::= .tok(locn,tk) | .endTok(locn).

  public tk ::= .idQTok(string)
    | .idTok(string)
    | .lftTok(string)
    | .rgtTok(string)
    | .intTok(integer)
    | .bigTok(bigint)
    | .fltTok(float)
    | .chrTok(char)
    | .strTok(cons[stringSegment]).

  public stringSegment ::= .segment(locn,string)
    | .interpolate(locn,cons[token],string)
    | .evaluate(locn,cons[token]).

  public implementation display[tk] => {
    disp(Tk) => case Tk in {
      | .idQTok(Id) => "''#(Id)''"
      | .idTok(Id) => "'#(Id)'"
      | .intTok(Ix) => disp(Ix)
      | .bigTok(Bx) => disp(Bx)
      | .fltTok(Dx) => disp(Dx)
      | .chrTok(Ch) => disp(Ch)
      | .strTok(S) => "\"#(dispSegments(S)*)\""
      | .lftTok(Id) where .bkt(LId,_,_,_,_) ?= isBracket(Id) => "#(LId)"
      | .rgtTok(Id) where .bkt(_,_,RId,_,_) ?= isBracket(Id) => "#(RId)"
    }
  }

  dispSegments:(cons[stringSegment]) => cons[string].
  dispSegments(Segs) => (Segs//disp).

  public implementation display[stringSegment] => {
    disp(Sg) => case Sg in {
      | .segment(_,S) => S
      | .interpolate(_,S,"") => "\$($(S))"
      | .interpolate(_,S,F) => "\$($(S)):#(F);"
      | .evaluate(_,S) => "\#($(S))"
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
      | .idQTok(Id1) => .idQTok(Id2).=Tk2 && Id1==Id2
      | .idTok(Id1) => .idTok(Id2).=Tk2 && Id1==Id2
      | .intTok(Ix1) => .intTok(Ix2).=Tk2 && Ix1==Ix2
      | .bigTok(Ix1) => .bigTok(Ix2).=Tk2 && Ix1==Ix2
      | .fltTok(Dx1) => .fltTok(Dx2).=Tk2 && Dx1==Dx2
      | .chrTok(C1) => .chrTok(C2).=Tk2 && C1==C2
      | .strTok(S1) => .strTok(S2).=Tk2 && S1==S2
      | .lftTok(S1) => .lftTok(S2).=Tk2 && S1==S2
      | .rgtTok(S1) => .rgtTok(S2).=Tk2 && S1==S2
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

  -- We define a tracking state to allow us to collect locations
  public tokenState ::= .tokenState(string,integer,integer,integer,cons[char]).

  atEof:(tokenState) => boolean.
  atEof(.tokenState(_,_,_,_,Str)) => _eof(Str).

  public nextChr:(tokenState) => (tokenState,option[char]).
  nextChr(St) where .tokenState(_,_,_,_,.cons(Ch,_)).=St  => (nxtSt(St),.some(Ch)).
  nextChr(St) default => (St,.none).

  public hedChar:(tokenState) => option[char].
  hedChar(.tokenState(_,_,_,_,Txt)) => head(Txt).

  public interSt:(tokenState,string) => tokenState.
  interSt(.tokenState(P,Ln,Cl,Off,_),Txt) => .tokenState(P,Ln,Cl,Off,Txt::cons[char]).

  public nxtSt:(tokenState) => tokenState.
  nxtSt(.tokenState(Pk,Line,Col,Off,.cons(`\n`,Txt))) =>
    .tokenState(Pk,Line+1,1,Off+1,Txt).
  nxtSt(.tokenState(Pk,Line,Col,Off,.cons(_,Txt))) =>
    .tokenState(Pk,Line,Col+1,Off+1,Txt).

  public lookingAt:(tokenState,cons[char]) => option[tokenState].
  lookingAt(St,[]) => .some(St).
  lookingAt(St,[Ch,..Nxt]) where Ch?=hedChar(St) => lookingAt(nxtSt(St),Nxt).
  lookingAt(_,_) default => .none.

  public makeLoc:(tokenState,tokenState)=>locn.
  makeLoc(.tokenState(Pk,Line,Col,Start,_),.tokenState(_,_,_,End,_)) =>
    .locn(Pk,Line,Col,Start,End-Start).

  public skipToNx:(tokenState) => tokenState.
  skipToNx(St) where Ch ?= hedChar(St) && isNonPrint(Ch) => skipToNx(nxtSt(St)).
  skipToNx(St) where Nx ?= lookingAt(St,[`-`,`-`,` `]) => skipToNx(lineComment(Nx)).
  skipToNx(St) where Nx ?= lookingAt(St,[`-`,`-`,`\n`]) => skipToNx(Nx).
  skipToNx(St) where Nx ?= lookingAt(St,[`-`,`-`,`\t`]) => skipToNx(lineComment(Nx)).
  skipToNx(St) where Nx ?= lookingAt(St,[`/`,`*`]) => skipToNx(blockComment(Nx)).
  skipToNx(St) => St.

  lineComment(St) where Ch?=hedChar(St) => ((Ch==`\n`||_isZlChar(Ch)) ?? nxtSt(St) || lineComment(nxtSt(St))).
  lineComment(St) => St.

  blockComment(St) where Nx?=lookingAt(St,[`*`,`/`]) => Nx.
  blockComment(St) where atEof(St) => St.
  blockComment(St) default => blockComment(nxtSt(St)).

  isNonPrint:(char) => boolean.
  isNonPrint(Ch) => (_isZlChar(Ch) || _isZsChar(Ch) || _isZpChar(Ch) || _isCcChar(Ch)).

  public implementation display[tokenState] => {
    disp(.tokenState(Pk,Line,Col,Off,_)) => disp(.locn(Pk,Line,Col,Off,0)).
  }

  public implementation hasLoc[tokenState] => {
    locOf(.tokenState(Pkg,Line,Col,Start,_)) => .some(.locn(Pkg,Line,Col,Start,0)).
  }
}
