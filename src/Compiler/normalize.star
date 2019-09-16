star.compiler.normalize{
  import star.

  import star.compiler.canon.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  nameMapEntry ::= moduleFun(string,tipe)
    | moduleVar(string,tipe)
    | localFun(string,tipe,string,integer,crVar)
    | localVar(string,tipe,string,integer,crVar)
    | moduleCons(string,tipe,string,integer)
    | labelArg(crVar,integer,crVar).

  mapLayer ::= lyr(string,map[string,nameMapEntry],crVar,option[crVar]).

  nameMap ~> list[mapLayer].

  implementation hasType[nameMapEntry]=>{.
    typeOf(moduleFun(_,Tp)) => Tp.
    typeOf(moduleVar(_,Tp)) => Tp.
    typeOf(localFun(_,Tp,_,_,_)) => Tp.
    typeOf(localVar(_,Tp,_,_,_)) => Tp.
    typeOf(moduleCons(_,Tp,_,_)) => Tp.
    typeOf(labelArg(_,_,V)) => typeOf(V).
  .}

  thetaMap:(canon,set[crVar],nameMap,reports) => either[reports,(nameMap,canon)].
  thetaMap(Theta,Q,Outer,Rp) => do{
    ThFree = findFreeVars(Theta,Outer,Q);
    logMsg("free vars = $(ThFree)");
    CellVars = cellVars(Theta);
    throw reportError(Rp,"not done",locOf(Theta))
  }

  findFreeVars:(canon,nameMap,set[crVar]) => set[crVar].
  findFreeVars(Term,Map,Q) => valof action{
    Df = definedProgs(Map);
    Lv = labelVars(Map);
    Q1 = Q\/Df\/Lv;
    ThFr = freeVarsInTerm(Term,Df,Q1);
    valis freeLabelVars(Q1,Map,ThFr)
  }

  definedProgs:(nameMap)=>set[crVar].
  definedProgs(Map) => let{
    defProgs([],Df) => Df.
    defProgs([lyr(_,Defs,_,_),..Mp],Df) =>
      defProgs(Mp,ixRight(definedInDefs,Df,Defs)).

    definedInDefs(Nm,Entry,Df) where definedP(Entry) =>
      (crId(Nm,_) in Df ?
	  Df ||
	  [crId(Nm,typeOf(Entry)),..Df]).
    definedInDefs(_,_,Df) => Df.

    definedP(moduleFun(_,_))=>true.
    definedP(moduleVar(_,_))=>true.
    definedP(_) default => false.
    
  } in defProgs(Map,[]).

  labelVars:(nameMap) => set[crVar].
  labelVars(Map) => lblVars(Map,[]).

  lblVars:(nameMap,set[crVar])=>set[crVar].
  lblVars([],Vrs) => Vrs.
  lblVars([lyr(_,Defs,_,none),..Map],Vrs) =>
    lblVars(Map,ixRight(labelVarsInDef,Vrs,Defs)).
  lblVars([lyr(_,Defs,_,some(ThVr)),..Map],Vrs) =>
    lblVars(Map,ixRight(labelVarsInDef,_addMem(ThVr,Vrs),Defs)).

  labelVarsIndef:(string,nameMapEntry,set[crVar])=>set[crVar].
  labelVarsInDef(_,labelArg(V,_,_),Vrs) => _addMem(V,Vrs).
  labelVarsInDef(_,_,Vrs) => Vrs.
 
  cellVars:(canon) => set[crVar].
  cellVars(theta(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellVar,CV,Ds),[],Defs).
  cellVars(record(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellVar,CV,Ds),[],Defs).

  pickCellVar:(canonDef,set[crVar])=>set[crVar].
  pickCellVar(varDef(Lc,Nm,_,_,_,Tp),CellVars) where isRefType(Tp) =>
    _addMem(crId(Nm,Tp),CellVars).
  pickCellVar(_,CellVars) => CellVars.

  freeLabelVars:(set[crVar],nameMap,set[crVar]) => set[crVar].
  freeLabelVars(Q,Map,Fr) =>
    foldRight((V,FF)=>lookupThetaVar(V,Map,FF),Fr,Q).

  lookupThetaVar:(crVar,nameMap,set[crVar]) => set[crVar].
  lookupThetaVar(crId(Nm,_),Map,FF) where
      ThVr^=lookup(Map,Nm,isLocal) => _addMem(ThVr,FF).
  lookupThetaVar(_,_,FF) default => FF.

  isLocal(localFun(_,_,_,_,ThVr))=>some(ThVr).
  isLocal(localVar(_,_,_,_,ThVr))=>some(ThVr).
  isLocal(_) default => none.

  lookup:all e ~~ (nameMap,string,(nameMapEntry)=>option[e])=>option[e].
  lookup([],_,_) => none.
  lookup([lyr(_,Entries,_,_),..Map],Nm,P) where E ^= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

  anyDef(D) => some(D).

  normExp:(canon,nameMap)=>crExp.
  normExp(vr(Lc,Nm,Tp),Map) => normVar(Lc,Nm,Tp,Map).
  normExp(litrl(Lc,Dt,Tp),_) => crLit(Lc,Dt,Tp).

  normVar:(locn,string,tipe,nameMap)=>crExp.
  normVar(Lc,Nm,Tp,Map) where V ^= lookupVarName(Map,Nm) =>
    implementVarExp(V,Lc,Nm,Tp,Map).

  implementVarExp(moduleVar(LNm,_),Lc,Nm,Tp,Map) => crVar(Lc,crId(LNm,Tp)).
      
  
}
