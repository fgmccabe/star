star.compiler.normalize{
  import star.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.freevars.

  import star.compiler.location.

  import star.compiler.terms.

  nameMapEntry ::=
    moduleFun(string,string,integer).

  mapLayer ::= lyr(string,map[string,nameMapEntry],term,term).

  nameMap ~> list[mapLayer].

  thetaMap:(canon,set[string],nameMap,reports) => either[reports,(nameMap,term)].
  thetaMap(Theta,Q,Outer,Rp) => do{
    ThFree = freeVarsInTerm(Theta,Q,

}
