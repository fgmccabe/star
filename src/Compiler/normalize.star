star.compiler.normalize{
  import star.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.types.

  import star.compiler.location.

  import star.compiler.terms.

  nameMapEntry ::= moduleFun(string,tipe,string,integer)
    | moduleVar(string,tipe,string,integer)
    | localFun(string,tipe,string,integer,canon)
    | localVar(string,tipe,string,integer,canon)
    | moduleCons(string,tipe,string,integer)
    | labelArg(canon,integer,canon).

  mapLayer ::= lyr(string,map[string,nameMapEntry],canon,option[canon]).

  nameMap ~> list[mapLayer].

  thetaMap:(canon,set[canon],nameMap,reports) => either[reports,(nameMap,canon)].
  thetaMap(Theta,Q,Outer,Rp) => do{
    ThFree = findFreeVars(Theta,Outer,Q);
    CellVars = cellVars(Theta);
    throw reportError("not done",locOf(Theta))
  }

  findFreeVars:(canon,nameMap,set[canon]) => set[canon].
  findFreeVars(Term,Map,Q) => valof action{
    Df = definedProgs(Map);
    Lv = labelVars(Map);
    Q1 = Q\/Df\/Lv;
    ThFr = freeVarsInTerm(Term,Df,Q1);
    valis freeLabelVars(Q1,Map,ThFr)
  }

  definedProgs:(nameMap)=>set[canon].
  definedProgs(Map) => let{
    defProgs([],Df) => Df.
    defProgs([lyr(_,Defs,_,_),..Mp],Df) =>
      defProgs(Mp,ixRight(definedInDefs,Df,Defs)).

    definedInDefs(Nm,Entry,Df) where definedP(Entry) =>
      (vr(_,Nm,_) in Df ?
	  Df ||
	  [vr(locOf(Entry),Nm,typeOf(Entry)),..Df]).
    definedInDefs(_,_,Df) => Df.

    definedP(moduleFun(_,_,_,_))=>true.
    definedP(moduleVar(_,_,_,_))=>true.
    definedP(_) default => false.
    
  } in defProgs(Map,[]).

  labelVars:(nameMap) => set[canon].
  labelVars(Map) => lblVars(Map,[]).

  lblVars([],Vrs) => Vrs.
  lblVars([lyr(_,Defs,_,none),..Map],Vrs) =>
    lblVars(Map,foldRight(labelVarsInDef,Vrs,Defs)).
  lblVars([lyr(_,Defs,_,some(ThVr)),..Map],Vrs) =>
    lblVars(Map,foldRight(labelVarsInDef,_addMem(ThVr,Vrs),Defs)).
  
  labelVarsInDef((_,labelArg(V,_,_)),Vrs) => _addMem(V,Vrs).
  labelVarsInDef(_,Vrs) => Vrs.
 
  cellVars:(canon) => list[canon].
  cellVars(theta(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellVar,CV,Ds),[],Defs).
--    foldRight(pickInDefs,[],Defs).
  cellVars(record(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellVar,CV,Ds),[],Defs).

--  pickInDefs:(list[canonDef],list[canon]) => list[canon].
--  pickInDefs(Ds,CV) => foldRight(pickCellVar,CV,Ds).

  pickCellVar:(canonDef,list[canon])=>list[canon].
  pickCellVar(varDef(Lc,Nm,_,_,_,Tp),CellVars) where isRefType(Tp) =>
    _addMem(vr(Lc,Nm,Tp),CellVars).
  pickCellVar(_,CellVars) => CellVars.

  freeLabelVars:(set[canon],nameMap,set[canon]) => set[canon].
  freeLabelVars(Q,Map,Fr) =>
    foldRight((V,FF)=>lookupThetaVar(V,Map,FF),Fr,Q).

  lookupThetaVar:(canon,nameMap,set[canon]) => set[canon].
  lookupThetaVar(vr(_,Nm,_),Map,FF) where
      ThVr^=lookup(Map,Nm,isLocal) => _addMem(FF,ThVr).
  lookupThetaVar(_,_,FF) default => FF.

  isLocal(localFun(_,_,_,_,ThVr))=>some(ThVr).
  isLocal(localVar(_,_,_,_,ThVr))=>some(ThVr).
  isLocal(_) default => none.

  lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => none.
  lookup([lyr(_,Entries,_,_),..Map],Nm,P) where E ^= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).
  

}
