star.compiler.normalize{
  import star.

  import star.compiler.canon.
  import star.compiler.core.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.terms.

  nameMapEntry ::= moduleFun(string,tipe)
    | moduleVar(string,tipe)
    | localFun(string,string,string,tipe,crVar)
    | localVar(string,string,string,tipe,crVar)
    | moduleCons(string,tipe)
    | labelArg(crVar,integer,crVar).

  mapLayer ::= lyr(string,map[string,nameMapEntry],crVar).

  nameMap ~> list[mapLayer].

  extraVars:(nameMap)=>list[crVar].
  extraVars([lyr(_,_,V),.._]) => [V].

  implementation hasType[nameMapEntry]=>{.
    typeOf(moduleFun(_,Tp)) => Tp.
    typeOf(moduleVar(_,Tp)) => Tp.
    typeOf(localFun(_,_,_,Tp,_)) => Tp.
    typeOf(localVar(_,_,_,Tp,_)) => Tp.
    typeOf(moduleCons(_,Tp)) => Tp.
    typeOf(labelArg(_,_,V)) => typeOf(V).
  .}

  liftTheta:(canon,set[crVar],nameMap,reports) => either[reports,crExp].
  liftTheta(Th,Q,Outer,Rp) => do{
    Map <- thetaMap(Th,Q,Outer,Rp);
    Defs <- transformThetaDefs(Th,Map,Outer,Rp);
    throw reportError(Rp,"not done",locOf(Th))
  }

  thetaMap:(canon,set[crVar],nameMap,reports) => either[reports,nameMap].
  thetaMap(Theta,Q,Outer,Rp) => do{
    ThFree = findFreeVars(Theta,Outer,Q);
    logMsg("free vars = $(ThFree)");
    CellVars = cellVars(Theta);
    FreeVars = CellVars++ThFree;
    ThVr = genVar("_ThVr",tupleType(FreeVars//typeOf));
    L = collectLabelVars(FreeVars,ThVr,0,[]);
    valis makeMtdMap(Theta,Outer,ThVr,L)
  }

  transformThetaDefs(theta(Lc,Path,Lbled,Defs,Others,Tp),Map,Outer,Rp) => do{
    for Dfs in Defs do{
      for D in Dfs do {
	transformThetaDef(D,Map,Outer,Rp);
	throw reportError(Rp,"not",Lc)
      }
    };
    throw reportError(Rp,"not done",Lc)
  }

  transformThetaDef(varDef(Lc,Nm,FullNm,lambda(Eqns,Tp),_,_),Map,Outer,Rp) => do{
    transformFunction(Eqns,Nm,FullNm,Lc,Tp,Map,Outer,Rp)
  }

  transformFunction:(list[equation],string,string,locn,tipe,
    nameMap,nameMap,reports) => either[reports,crExp].
  transformFunction(Eqns,Nm,FullNm,Lc,Tp,Map,Outer,Rp) => do{
    Extra = extraVars(Map);
    ATp = extendFunTp(deRef(Tp),Extra);
    LclPrg = lbl(FullNm,arity(ATp));
    Eqs <- transformEquations(Eqns,Map,Outer,Extra,Rp);
    throw reportError(Rp,"not done",Lc)
  }

  transformEquation(eqn(Lc,tple(_,As),Val),Map,Outer,Extra,Rp) => do{
    
  }

  extendFunTp(Tp,[]) => Tp.
  extendFunTp(Tp,Vs) where (A,B)^=isFunType(Tp) &&
      tupleType(Es).=deRef(A) =>
    funType(tupleType(extendTplType(Es,Vs)),B).
  extendFunTp(allType(V,B),Vs) => allType(V,extendFunTp(B,Vs)).
  extendFunTp(existType(V,B),Vs) => existType(V,extendFunTp(B,Vs)).
  extendFunTp(constrainedType(T,C),Vs) => constrainedType(extendFunTp(T,Vs),C).

  extendTplType:(list[tipe],list[crVar]) => list[tipe].
  extendTplType(Es,[]) => Es.
  extendTplType(Es,[V,..Vs]) => [typeOf(V),..extendTplType(Es,Vs)].

  makeMtdMap(theta(_,Nm,_,Defs,_,_),Outer,ThVr,L) =>
    [lyr(Nm,collectMtds(Defs,layerName(Outer),ThVr,L),ThVr),..Outer].

  collectMtds(Defs,Outer,ThVr,L) =>
    foldRight((Dfs,Li)=>foldRight((D,LL)=>collectMtd(D,Outer,ThVr,LL),Li,Dfs),L,Defs).

  collectMtd:(canonDef,string,crVar,map[string,nameMapEntry])=>map[string,nameMapEntry].
  collectMtd(varDef(Lc,Nm,FullNm,_,_,Tp),Outer,ThVr,LL) =>
    LL[Nm->localVar(FullNm,qualifiedName(Outer,"%",Nm),qualifiedName(Outer,"^",Nm),Tp,ThVr)].
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),Outer,ThVr,LL) =>
    LL[Nm->moduleCons(qualifiedName(Outer,"#",Nm),Tp)].
  collectMtd(_,_,_,LL) default => LL.

  findFreeVars:(canon,nameMap,set[crVar]) => list[crVar].
  findFreeVars(Term,Map,Q) => valof action{
    Df = definedProgs(Map);
    Lv = labelVars(Map);
    Q1 = Q\/Df\/Lv;
    ThFr = freeVarsInTerm(Term,Df,Q1);
    valis freeLabelVars(Q1,Map,ThFr)::list[crVar]
  }

  collectLabelVars:(list[crVar],crVar,integer,map[string,nameMapEntry]) => map[string,nameMapEntry].
  collectLabelVars([],_,_,LV) => LV.
  collectLabelVars([V,..Vrs],ThV,Ix,Entries) where crId(Nm,_) .= V =>
    collectLabelVars(Vrs,ThV,Ix+1,Entries[Nm->labelArg(ThV,Ix,V)]).

  makeFreeTerm:(set[crVar],set[crVar],nameMap,locn) => crExp.
  makeFreeTerm(CellVars,FreeVrs,Map,Lc) =>
    mkCrTpl(foldRight((V,FV) => [FV..,crVar(Lc,V)],
	foldRight((V,FV)=>[FV..,emptyCell(V,Lc)],[],CellVars),FreeVrs),Lc).

  emptyCell(V,Lc) => crVar(Lc,V).

  definedProgs:(nameMap)=>set[crVar].
  definedProgs(Map) => let{
    defProgs([],Df) => Df.
    defProgs([lyr(_,Defs,_),..Mp],Df) =>
      defProgs(Mp,ixRight(definedInDefs,Df,Defs)).

    definedInDefs(Nm,Entry,Df) where definedP(Entry) =>
      (crId(Nm,_) in Df ?
	  Df ||
	  [crId(Nm,typeOf(Entry)),..Df]).
    definedInDefs(_,_,Df) => Df.

    definedP(moduleFun(_,_))=>true.
    definedP(moduleVar(_,_))=>true.
    definedP(localVar(_,_,_,_,_))=>true.
    definedP(_) default => false.
  } in defProgs(Map,[]).

  labelVars:(nameMap) => set[crVar].
  labelVars(Map) => lblVars(Map,[]).

  lblVars:(nameMap,set[crVar])=>set[crVar].
  lblVars([],Vrs) => Vrs.
  lblVars([lyr(_,Defs,ThVr),..Map],Vrs) =>
    lblVars(Map,ixRight(labelVarsInDef,_addMem(ThVr,Vrs),Defs)).

  labelVarsIndef:(string,nameMapEntry,set[crVar])=>set[crVar].
  labelVarsInDef(_,labelArg(V,_,_),Vrs) => _addMem(V,Vrs).
  labelVarsInDef(_,_,Vrs) => Vrs.

  cellDefs:(canon) => list[canonDef].
  cellDefs(theta(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellDef,CV,Ds),[],Defs).
  cellDefs(record(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellDef,CV,Ds),[],Defs).

  pickCellDef:(canonDef,list[canonDef])=>list[canonDef].
  pickCellDef(Df where varDef(Lc,Nm,_,_,_,Tp).=Df,CellDefs) where isRefType(Tp) =>
    [CellDefs..,Df].
  pickCellDef(_,CellDefs) => CellDefs.
  
  cellVars:(canon) => list[crVar].
  cellVars(theta(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellVar,CV,Ds),[],Defs).
  cellVars(record(_,_,_,Defs,_,_)) =>
    foldRight((Ds,CV)=>foldRight(pickCellVar,CV,Ds),[],Defs).

  pickCellVar:(canonDef,list[crVar])=>list[crVar].
  pickCellVar(varDef(Lc,Nm,_,_,_,Tp),CellVars) where isRefType(Tp) =>
    [crId(Nm,Tp),..CellVars].
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
  lookup([lyr(_,Entries,_),..Map],Nm,P) where E ^= Entries[Nm] =>
    P(E).
  lookup([_,..Map],Nm,P) => lookup(Map,Nm,P).

  lookupVarName:(nameMap,string)=>option[nameMapEntry].
  lookupVarName(Map,Nm) => lookup(Map,Nm,anyDef).

  layerName:(nameMap) => string.
  layerName([lyr(Nm,_,_),.._]) => Nm.

  anyDef(D) => some(D).

  normExp:(canon,nameMap)=>crExp.
  normExp(vr(Lc,Nm,Tp),Map) => normVar(Lc,Nm,Tp,Map).
  normExp(litrl(Lc,Dt,Tp),_) => crLit(Lc,Dt,Tp).

  normVar:(locn,string,tipe,nameMap)=>crExp.
  normVar(Lc,Nm,Tp,Map) where V ^= lookupVarName(Map,Nm) =>
    implementVarExp(V,Lc,Nm,Tp,Map).

  implementVarExp(moduleVar(LNm,_),Lc,Nm,Tp,Map) => crVar(Lc,crId(LNm,Tp)).

  genVar:(string,tipe) => crVar.
  genVar(Pr,Tp) => crId(genSym(Pr),Tp).
  
}
