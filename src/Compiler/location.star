star.compiler.location{
  import star.
  import star.pkg.

  public locn ::= locn(pkg,integer,integer,integer,integer).

  public implementation display[locn] => {.
    disp(locn(pkg(P,_),Line,Col,Off,Ln)) => ssSeq([ss(P),ss(":"),disp(Line),ss(":"),disp(Col),ss("("),disp(Ln),ss(")")]).
  .}

  public implementation equality[locn] => {.
    locn(P1,_,_,St1,Ln1) == locn(P2,_,_,St2,Ln2) => P1==P2 && St1==St2 && Ln1==Ln2.
  .}

  public implementation hash[locn] => {.
    hash(locn(P,_,_,Off,Len)) => (((hash(P)*37)+hash(Off))*37)+hash(Len).
  .}

  public contract all t ~~ hasLoc[t] ::= {
    locOf:(t)=>locn.
  }

  public mergeLoc:(locn,locn) => locn.
  mergeLoc(locn(P,L1,C1,S1,Ln1),locn(P,L2,C2,S2,Ln2)) =>
    (S1>S2 ? locn(P,L2,C2,S2,S1-S2+Ln1) || locn(P,L1,C1,S1,S2-S1+Ln2)).

  public pkgLoc:(pkg)=>locn.
  pkgLoc(P) => locn(P,1,0,0,0).
}
