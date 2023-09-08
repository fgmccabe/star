star.location{
  import star.

  public locn ::= .locn(string,integer,integer,integer,integer).

  public contract all t ~~ hasLoc[t] ::= {
    locOf:(t)=>option[locn].
  }

  public implementation display[locn] => {
    disp(.locn(P,Line,Col,_,Ln)) => "#(P)\:$(Line)\:$(Col)($(Ln))".
  }

  public implementation coercion[locn,string] => {
    _coerce(Lc) => disp(Lc):?string
  }
    
  public implementation equality[locn] => {
    .locn(P1,_,_,St1,Ln1) == .locn(P2,_,_,St2,Ln2) => P1==P2 && St1==St2 && Ln1==Ln2.
  }

  public implementation hashable[locn] => {
    hash(.locn(P,_,_,Off,Len)) => (((hash(P)*37)+hash(Off))*37)+hash(Len).
  }

  public mergeLoc:(option[locn],option[locn]) => option[locn].
  mergeLoc(.none,L) => L.
  mergeLoc(L,.none) => L.
  mergeLoc(.some(.locn(P,L1,C1,S1,Ln1)),.some(.locn(P,L2,C2,S2,Ln2))) =>
    .some(S1>S2 ?? .locn(P,L2,C2,S2,S1-S2+Ln1) || .locn(P,L1,C1,S1,S2-S1+Ln2)).

  public locPos(.locn(_,_,_,Pos,_)) => Pos.

  public locPkg(.locn(P,_,_,_,_)) => P.

  public startLoc(Pth) => .locn(Pth,1,0,0,0).
  
}
