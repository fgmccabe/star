star.compiler.canondeps{
  import star.

  import star.compiler.canon.
  import star.compiler.core.
  import star.compiler.freevars.
  import star.topsort.
  import star.compiler.meta.

  public sortDefs:(list[canonDef])=>list[list[canonDef]].
  sortDefs(Defs) => valof action{
    Defined .= foldRight((D,S)=>_addMem(definedName(D),S),[],Defs);
--    logMsg("defined entries $(Defined)");
    AllRefs .= foldRight((D,A) => [A..,findRefs(D,D,Defined)],([]:list[defSpec]),Defs);
--    logMsg("all refs $(AllRefs)");

    Sorted .= topsort(AllRefs);
--    logMsg("groups: $(Sorted)");
    valis (Sorted // ((Gp)=>(Gp//((defSpec(_,_,Df))=>Df))))
  }

  definedName:(canonDef)=>defnSp.
  definedName(varDef(_,Nm,_,_,_,_))=>varSp(Nm).
  definedName(typeDef(_,Nm,_,_))=>tpSp(Nm).
  definedName(conDef(_,Nm,_,_))=>conSp(Nm).
  definedName(cnsDef(_,Nm,_,_))=>varSp(Nm).
  definedName(implDef(_,_,Nm,_,_,_))=>varSp(Nm).

  defSpec ::= defSpec(defnSp,list[defnSp],canonDef).

  implementation depends[defSpec->>defnSp] => {
    references(defSpec(_,Refs,_)) => Refs.
    defined(defSpec(Sp,_,_),Rf) => Sp==Rf.
  }

  implementation display[defSpec] => {.
    disp(defSpec(V,R,_)) => ssSeq([disp(V),ss(" -> "),disp(R),ss("\n")]).
  .}
  
  findRefs:(canonDef,canonDef,set[defnSp])=>defSpec.
  findRefs(varDef(_,Nm,_,Val,_,_),D,All) => defSpec(varSp(Nm),freeRefs(Val,All),D).
  findRefs(typeDef(_,Nm,_,_),D,All) => defSpec(tpSp(Nm),[],D).
  findRefs(conDef(_,Nm,_,_),D,All) => defSpec(conSp(Nm),[],D).
  findRefs(cnsDef(_,Nm,_,_),D,All) => defSpec(cnsSp(Nm),[],D).
  findRefs(implDef(_,_,Nm,Val,_,_),D,All) => defSpec(varSp(Nm),freeRefs(Val,All),D).

  freeRefs(Val,All) => let{
    Free = freeVarsInTerm(Val,[],[]).
  } in [varSp(Nm) | crId(Nm,_) in Free && varSp(Nm) in All].
}
