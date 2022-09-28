star.compiler.term.deps{
  import star.

  import star.compiler.data.
  import star.compiler.term.
  import star.compiler.types.
  import star.topsort.

  public sortDefs:(cons[cDefn])=>cons[cons[cDefn]].
  sortDefs(Defs) => valof{
    Defined = foldRight((D,S)=>S\+definedName(D),[],Defs);
    Q = foldRight(pickVar,[],Defined);
    AllRefs = foldRight((D,A) => [findRefs(D,D,Q,Defined),..A],([]:cons[defSpec]),Defs);
    Sorted = topsort(AllRefs);
    valis (Sorted // ((Gp)=>(Gp//((defSpec(_,_,Df))=>Df))))
  }

  defnSp ::= varSp(string,tipe) | lblSp(termLbl) | tpSp(string).

  implementation equality[defnSp] => {
    varSp(V1,T1) == varSp(V2,T2) => V1==V2 && T1==T2.
    lblSp(L1) == lblSp(L2) => L1==L2.
    tpSp(T1) == tpSp(T2) => T1==T2.
    _ == _ default => .false.
  }

  implementation hashable[defnSp] => {
    hash(varSp(V,T)) => hash((V,T)).
    hash(lblSp(L)) => hash(L).
    hash(tpSp(T)) => hash(T).
  }

  implementation display[defnSp] => {
    disp(varSp(V,T)) => "var: $(V)\:$(T)".
    disp(lblSp(L)) => "lbl: $(L)".
    disp(tpSp(T)) => "type: $(T)".
  }

  pickVar(varSp(V,T),Q) => Q\+cId(V,T).
  pickVar(_,Q) => Q.
    
  definedName:(cDefn)=>defnSp.
  definedName(fnDef(_,Nm,Tp,_,_))=>varSp(Nm,Tp).
  definedName(vrDef(_,Nm,Tp,_))=>varSp(Nm,Tp).
  definedName(tpDef(_,Tp,_,_))=>tpSp(tpName(Tp)).
  definedName(lblDef(_,Lbl,_,_))=>lblSp(Lbl).

  defSpec ::= defSpec(defnSp,cons[defnSp],cDefn).

  implementation depends[defSpec->>defnSp] => {
    references(defSpec(_,Refs,_)) => Refs.
    defined(defSpec(Sp,_,_),Rf) => Sp==Rf.
  }

  implementation display[defSpec] => {
    disp(defSpec(V,R,_)) => "$(V) -> $(R)".
  }
  
  findRefs:(cDefn,cDefn,set[cId],set[defnSp])=>defSpec.
  findRefs(fnDef(_,Nm,Tp,Args,Val),D,Q,All) =>
    defSpec(varSp(Nm,Tp),freeRefs(Val,foldLeft(((S,QQ) => QQ\-S),Q,Args),All),D).
  findRefs(vrDef(_,Nm,Tp,Val),D,Q,All) =>
    defSpec(varSp(Nm,Tp),freeRefs(Val,Q\-cId(Nm,Tp),All),D).
  findRefs(tpDef(_,Tp,_,_),D,_,All) => defSpec(tpSp(tpName(Tp)),[],D).
  findRefs(lblDef(_,Lbl,_,_),D,_,All) => defSpec(lblSp(Lbl),[],D).

  freeRefs(Val,Q,All) => let{
    Free = freeVarsInTerm(Val,Q).
  } in {varSp(V,T) | cId(V,T) in Free}.

  freeVarsInTerm(Val,Q) => let{
    checkTerm:(cExp,(set[cId],set[cId])) => (set[cId],set[cId]).
    checkTerm(cLtt(_,Vr,_,_),(FF,QQ)) => (FF,QQ\-Vr).
    checkTerm(cVar(_,Vr),(FF,QQ)) where Vr .<. QQ => (FF\+Vr,QQ).
    checkTerm(cCall(_,Nm,_,Tp),(FF,QQ)) where cId(Nm,Tp) .<. QQ => (FF\+cId(Nm,Tp),QQ).
    checkTerm(_,Fr) default => Fr.
    
    checkAct:(aAction,(set[cId],set[cId])) => (set[cId],set[cId]).
    checkAct(aDefn(_,P,_),(FF,QQ)) => (FF,QQ\ptnVars(P,[])).
    checkAct(aLtt(_,Vr,_,_),(FF,QQ)) => (FF,QQ\-Vr).
    checkAct(_,Fr) default => Fr.
  } in fst(visitE(Val,checkTerm,checkAct,([],Q))).
}
