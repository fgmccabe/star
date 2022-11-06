star.compiler.canondeps{
  import star.

  import star.compiler.canon.
  import star.compiler.freevars.
  import star.compiler.term.
  import star.compiler.types.
  import star.topsort.

  public sortDefs:(cons[canonDef])=>cons[cons[canonDef]].
  sortDefs(Defs) => valof{
    Defined = foldRight((D,S)=>S\+definedName(D),[],Defs);
    Q = foldRight(pickVar,[],Defined);
    AllRefs = foldRight((D,A) => [findRefs(D,D,Q,Defined),..A],([]:cons[defSpec]),Defs);
    Sorted = topsort(AllRefs);
    valis (Sorted // ((Gp)=>(Gp//((.defSpec(_,_,Df))=>Df))))
  }

  defnSp ::= varSp(string,tipe) | tpSp(tipe).

  implementation equality[defnSp] => {
    .varSp(V1,T1) == .varSp(V2,T2) => V1==V2 && T1==T2.
    .tpSp(T1) == .tpSp(T2) => T1==T2.
    _ == _ default => .false.
  }

  implementation hashable[defnSp] => {
    hash(.varSp(V,T)) => hash((V,T)).
    hash(.tpSp(T)) => hash(T).
  }

  implementation display[defnSp] => {
    disp(.varSp(V,T)) => "var: $(V)\:$(T)".
    disp(.tpSp(T)) => "type: $(T)".
  }

  pickVar(.varSp(V,T),Q) => Q\+.cId(V,T).
  pickVar(_,Q) => Q.
    
  definedName:(canonDef)=>defnSp.
  definedName(.varDef(_,Nm,_,_,_,Tp))=>.varSp(Nm,Tp).
  definedName(.typeDef(_,Nm,_,_))=>.tpSp(.nomnal(Nm)).
  definedName(.conDef(_,Nm,_,_))=>.tpSp(.nomnal(Nm)).
  definedName(.cnsDef(_,Nm,_,Tp))=>.varSp(Nm,Tp).
  definedName(.implDef(_,_,Nm,_,_,Tp))=>.varSp(Nm,Tp).

  defSpec ::= defSpec(defnSp,cons[defnSp],canonDef).

  implementation depends[defSpec->>defnSp] => {
    references(.defSpec(_,Refs,_)) => Refs.
    defined(.defSpec(Sp,_,_),Rf) => Sp==Rf.
  }

  implementation display[defSpec] => {
    disp(.defSpec(V,R,_)) => "$(V) -> $(R)".
  }
  
  findRefs:(canonDef,canonDef,set[cId],set[defnSp])=>defSpec.
  findRefs(Df,D,Q,All) => case Df in {
    .varDef(_,Nm,_,Val,_,Tp) => .defSpec(.varSp(Nm,Tp),freeRefs(Val,Q,All),D).
    .typeDef(_,Nm,_,_) => .defSpec(.tpSp(.nomnal(Nm)),[],D).
    .conDef(_,Nm,_,_) => .defSpec(.tpSp(.nomnal(Nm)),[],D).
    .cnsDef(_,Nm,_,Tp) => .defSpec(.varSp(Nm,Tp),[],D).
    .implDef(_,_,Nm,Val,_,Tp) => .defSpec(.varSp(Nm,Tp),freeRefs(Val,Q,All),D).
  }

  freeRefs(Val,Q,All) => let{
    Free = findFree(Val,[],Q).
  } in { .varSp(V,T) | .cId(V,T) in Free}.
}
