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

  defnSp ::= .varSp(string,tipe) | .tpSp(tipe).

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

  pickVar(.varSp(V,T),Q) => Q\+.cV(V,T).
  pickVar(_,Q) => Q.
    
  definedName:(canonDef)=>defnSp.
  definedName(.funDef(_,Nm,_,_,Tp))=>.varSp(Nm,Tp).
  definedName(.varDef(_,Nm,_,_,Tp))=>.varSp(Nm,Tp).
  definedName(.typeDef(_,Nm,_,_))=>.tpSp(.nomnal(Nm)).
  definedName(.cnsDef(_,Nm,_,Tp))=>.varSp(Nm,Tp).
  definedName(.implDef(_,_,Nm,_,_,Tp))=>.varSp(Nm,Tp).

  defSpec ::= .defSpec(defnSp,cons[defnSp],canonDef).

  implementation depends[defSpec->>defnSp] => {
    references(.defSpec(_,Refs,_)) => Refs.
    defined(.defSpec(Sp,_,_),Rf) => Sp==Rf.
  }

  implementation display[defSpec] => {
    disp(.defSpec(V,R,_)) => "$(V) -> $(R)".
  }
  
  findRefs:(canonDef,canonDef,set[cV],set[defnSp])=>defSpec.
  findRefs(Df,D,Q,All) => case Df in {
    | .funDef(_,Nm,Rls,_,Tp) => valof{
      Free = foldRight((Rl,F)=>freeVarsInRule(Rl,Q,F),[],Rls);
      valis .defSpec(.varSp(Nm,Tp),{ .varSp(V,T) | .cV(V,T) in Free},D)
    }
    | .varDef(_,Nm,Val,_,Tp) => .defSpec(.varSp(Nm,Tp),freeRefs(Val,Q,All),D)
    | .cnsDef(_,Nm,_,Tp) => .defSpec(.varSp(Nm,Tp),[],D)
    | .implDef(_,_,Nm,Val,_,Tp) => .defSpec(.varSp(Nm,Tp),freeRefs(Val,Q,All),D)
  }

  freeRefs(Val,Q,All) => let{
    Free = findFree(Val,Q).
  } in { .varSp(V,T) | .cV(V,T) in Free}.
}
