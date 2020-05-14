star.compiler.canondeps{
  import star.

  import star.compiler.canon.
  import star.compiler.core.
  import star.compiler.freevars.
  import star.compiler.types.
  import star.topsort.

  public sortDefs:(cons[canonDef])=>cons[cons[canonDef]].
  sortDefs(Defs) => valof action{
--    logMsg("dependency sort of $(Defs)");
    Defined .= foldRight((D,S)=>S\+definedName(D),[],Defs);
    Q .= foldRight(pickVar,[],Defined);
--    logMsg("defined entries Q=$(Q), $(Defined)");
    AllRefs .= foldRight((D,A) => [findRefs(D,D,Q,Defined),..A],([]:cons[defSpec]),Defs);
--    logMsg("all refs $(AllRefs)");

    Sorted .= topsort(AllRefs);
--    logMsg("canon groups: $(Sorted)");
    valis (Sorted // ((Gp)=>(Gp//((defSpec(_,_,Df))=>Df))))
  }

  defnSp ::= varSp(crVar) | tpSp(tipe).

  implementation equality[defnSp] => {.
    varSp(V1) == varSp(V2) => V1==V2.
    tpSp(T1) == tpSp(T2) => T1==T2.
    _ == _ default => .false.
  .}

  implementation hash[defnSp] => {.
    hash(varSp(V1)) => hash(V1).
    hash(tpSp(T1)) => hash(T1).
  .}

  implementation display[defnSp] => {.
    disp(varSp(V)) => ssSeq([ss("var:"),disp(V)]).
    disp(tpSp(T)) => ssSeq([ss("type:"),disp(T)]).
  .}

  pickVar(varSp(V),Q) => Q\+V.
  pickVar(_,Q) => Q.
    
  definedName:(canonDef)=>defnSp.
  definedName(varDef(_,Nm,_,_,_,Tp))=>varSp(crId(Nm,Tp)).
  definedName(typeDef(_,Nm,_,_))=>tpSp(nomnal(Nm)).
  definedName(conDef(_,Nm,_,_))=>tpSp(nomnal(Nm)).
  definedName(cnsDef(_,Nm,_,Tp))=>varSp(crId(Nm,Tp)).
  definedName(implDef(_,_,Nm,_,_,Tp))=>varSp(crId(Nm,Tp)).

  defSpec ::= defSpec(defnSp,cons[defnSp],canonDef).

  implementation depends[defSpec->>defnSp] => {
    references(defSpec(_,Refs,_)) => Refs.
    defined(defSpec(Sp,_,_),Rf) => Sp==Rf.
  }

  implementation display[defSpec] => {.
    disp(defSpec(V,R,_)) => ssSeq([disp(V),ss(" -> "),disp(R),ss("\n")]).
  .}
  
  findRefs:(canonDef,canonDef,set[crVar],set[defnSp])=>defSpec.
  findRefs(varDef(_,Nm,_,Val,_,Tp),D,Q,All) => defSpec(varSp(crId(Nm,Tp)),freeRefs(Val,Q,All),D).
  findRefs(typeDef(_,Nm,_,_),D,_,All) => defSpec(tpSp(nomnal(Nm)),[],D).
  findRefs(conDef(_,Nm,_,_),D,_,All) => defSpec(tpSp(nomnal(Nm)),[],D).
  findRefs(cnsDef(_,Nm,_,Tp),D,Q,All) => defSpec(varSp(crId(Nm,Tp)),[],D).
  findRefs(implDef(_,_,Nm,Val,_,Tp),D,Q,All) => defSpec(varSp(crId(Nm,Tp)),freeRefs(Val,Q,All),D).

  freeRefs(Val,Q,All) => let{
    Free = freeVarsInTerm(Val,[],Q,[]).
  } in {varSp(V) | V in Free}.
}
