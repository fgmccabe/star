star.compiler.location{
  import star.
  import star.pkg.
  public import star.location.

  public contract all t ~~ hasLoc[t] ::= {
    locOf:(t)=>locn.
  }

  public mergeLoc:(locn,locn) => locn.
  mergeLoc(locn(P,L1,C1,S1,Ln1),locn(P,L2,C2,S2,Ln2)) =>
    (S1>S2 ? locn(P,L2,C2,S2,S1-S2+Ln1) || locn(P,L1,C1,S1,S2-S1+Ln2)).

}
