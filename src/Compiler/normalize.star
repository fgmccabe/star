star.compiler.normalize{
  import star.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.freevars.

  import star.compiler.location.

  import star.compiler.terms.

  nameMapEntry ::= moduleFun(string,string,integer)
    | moduleCons(string,string,integer)
    | labelArg(canon,integer,canon).

  mapLayer ::= lyr(string,map[string,nameMapEntry],canon,option[canon]).

  nameMap ~> list[mapLayer].

  thetaMap:(canon,set[canon],nameMap,reports) => either[reports,(nameMap,canon)].
  thetaMap(Theta,Q,Outer,Rp) => do{
    ThFree = findFreeVars(Theta,Outer,Q);
    CellVars = cellVars(Theta);
  }

  findFreeVars:(canon,nameMap,set[string]) => list[canon].
  findFreeVars(Term,Map,Q) => valof action{
    Df = defineProgs(Map);
    Lv = labelVars(Map);
    Q1 = Q\/Df\/Lv;
    ThFr = freeVars(Term,Df,Q1);
    valis freeLabelVars(Q1,Map,ThFr)
  }

  labelVars:(nameMap) => list[canon].
  labelVars(Map) => lblVars(Map,[]).

  lblVars([],Vrs) => Vrs.
  lblVars([lyr(_,Defs,_,none),..Map],Vrs) =>
    lblVars(Map,labelVarsInDefs(Defs,Vrs)).
  lblVars([lyr(_,Defs,_,some(ThVr)),..Map],Vrs) =>
    lblVars(Map,foldRight(labelVarsInDef,Defs,_addMem(ThVr,Vrs))).

  labelVarsInDef((_,labelArg(V,_,_)),Vrs) => _addMem(V,Vrs).
  labelVarsInDef(_,Vrs) => Vrs.
 
    
  cellVars:(canon) => list[canon].
  cellVars(theta(_,_,_,Defs,_,_,_)) =>
    foldRight(pickCellVar,[],Defs).
  cellVars(record(_,_,_,Defs,_,_,_)) =>
    foldRight(pickCellVar,[],Defs).

  pickCellVar(varDef(Lc,Nm,_,_,_,Tp),CellVars) where isRefType(Tp) =>
    _addMem(vr(Lc,Nm,Tp),CellVars).
  pickCellVar(varDef(Lc,Nm,_,_,_,Tp),CellVars) => CellVars.
  

}
