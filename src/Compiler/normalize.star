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

  thetaMap:(canon,set[canon],nameMap,reports) => either[reports,(nameMap,canon)].
  thetaMap(Theta,Q,Outer,Rp) => do{
    ThFree = findFreeVars(Theta,Outer,Q);
    CellVars = cellVars(Theta);
  }

  findFreeVars:(canon,nameMap,set[string]) => list[canon].
  findFreeVars(Term,Map,Q) => do{
    Df = defineProgs(Map);
    Lv = labelVars(Map);
  }

  cellVars:(canon) => list[canon].
  cellVars(theta(_,_,_,Defs,_,_,_)) =>
    foldRight(pickCellVar,[],Defs).
  cellVars(record(_,_,_,Defs,_,_,_)) =>
    foldRight(pickCellVar,[],Defs).

  pickCellVar(varDef(Lc,Nm,_,_,_,Tp),CellVars) where isRefType(Tp) =>
    _addMem(vr(Lc,Nm,Tp),CellVars).
  pickCellVar(varDef(Lc,Nm,_,_,_,Tp),CellVars) => CellVars.
  

}
