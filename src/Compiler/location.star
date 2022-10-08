star.compiler.location{
  import star.
  import star.pkg.
  public import star.location.

  public contract all t ~~ hasLoc[t] ::= {
    locOf:(t)=>option[locn].
  }

  public mergeLoc:(option[locn],option[locn]) => option[locn].
  mergeLoc(.none,L) => L.
  mergeLoc(L,.none) => L.
  mergeLoc(.some(.locn(P,L1,C1,S1,Ln1)),.some(.locn(P,L2,C2,S2,Ln2))) =>
    .some(S1>S2 ? .locn(P,L2,C2,S2,S1-S2+Ln1) || .locn(P,L1,C1,S1,S2-S1+Ln2)).
}
