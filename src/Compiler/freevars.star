star.compiler.freevars{
  import star.
  import star.sets.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.escapes.
  import star.compiler.misc.
  import star.compiler.term.
  import star.compiler.types.

  public contract all e ~~ freevars[e] ::= {
    findFree:(e,set[cV]) => set[cV].
  }

  public implementation freevars[canon] => {
    findFree(E,Q) => freeVarsInExp(E,Q,[])
  }

  public freeVarsInExp:(canon,set[cV],set[cV]) => set[cV].
  freeVarsInExp(Exp,Q,Fv) => case Exp in {
    | .anon(_,_) => Fv
    | .vr(Lc,Nm,Tp) where {? .cV(Nm,_) in Fv ?} => Fv
    | .vr(_,Nm,_) where isEscape(Nm) => Fv
    | .vr(Lc,Nm,Tp) => ({? .cV(Nm,_) in Q ?} ?? Fv\+.cV(Nm,Tp) || Fv)
    | .intr(_,_) => Fv
    | .bintr(_,_) => Fv
    | .kar(_,_) => Fv
    | .flt(_,_) => Fv
    | .strng(_,_) => Fv
    | .enm(_,_,_) => Fv
    | .dot(_,Rc,_,_) => freeVarsInExp(Rc,Q,Fv)
    | .tdot(_,Rc,_,_) => freeVarsInExp(Rc,Q,Fv)
    | .mtd(_,_,_) => Fv
    | .over(_,V,_) => freeVarsInExp(V,Q,Fv)
    | .csexp(_,G,Cs,_) =>
      foldLeft((Rl,F)=>freeVarsInRule(Rl,Q,F), freeVarsInExp(G,Q,Fv), Cs)
    | .cond(_,T,L,R) => valof{
      Q1 = Q\condVars(T,[]);
      valis freeVarsInExp(L,Q1,freeVarsInExp(R,Q,freeVarsInCond(T,Q1,Fv)))
    }
    | .apply(_,O,A,_) => freeVarsInTuple(A,Q,freeVarsInExp(O,Q,Fv))
    | .tapply(_,O,A,_,_) => freeVarsInTuple(A,Q,freeVarsInExp(O,Q,Fv))
    | .tple(_,Els) => freeVarsInTuple(Els,Q,Fv)
    | .match(_,P,S) where Q1 .= dropVars(P,Q) => freeVarsInExp(S,Q1,freeVarsInExp(P,Q1,Fv))
    | .conj(_,L,R) => freeVarsInCond(Exp,Q,Fv)
    | .disj(_,L,R) => freeVarsInCond(Exp,Q,Fv)
    | .neg(_,R) => freeVarsInCond(Exp,Q,Fv)
    | .trycatch(_,E,_,H,_) => freeVarsInExp(E,Q,foldRight((Rl,F)=>freeVarsInRule(Rl,Q,F),Fv,H))
    | .thrw(_,E,_,_) => freeVarsInExp(E,Q,Fv)
    | .lambda(_,_,Rl,_) => freeVarsInRule(Rl,Q,Fv)
    | .thunk(_,E,_) => freeVarsInExp(E,Q,Fv)
    | .thRef(_,E,_) => freeVarsInExp(E,Q,Fv)
    | .newSav(_,_) => Fv
    | .svGet(_,V,_) => freeVarsInExp(V,Q,Fv)
    | .svSet(_,S,V) => freeVarsInExp(S,Q,freeVarsInExp(V,Q,Fv))
    | .cell(_,V,_) => freeVarsInExp(V,Q,Fv)
    | .get(_,V,_) => freeVarsInExp(V,Q,Fv)
    | .letExp(_,D,_,E) => freeVarsInExp(E,dropDefs(D,Q),freeVarsInDefs(D,Q,Fv))
    | .letRec(Lc,D,_,E) => valof{
      QD = dropDefs(D,Q);
      valis freeVarsInExp(E,QD,freeVarsInDefs(D,QD,Fv))
    }
    | .vlof(_,A,_) => freeVarsInAct(A,Q,Fv)
    | .resum(_,T,M,_) => freeVarsInExp(T,Q,freeVarsInExp(T,Q,Fv))
    | .susp(_,T,M,_) => freeVarsInExp(T,Q,freeVarsInExp(T,Q,Fv))
    | .retyre(_,T,M,_) => freeVarsInExp(T,Q,freeVarsInExp(T,Q,Fv))
    | _ default => valof{
      reportError("cant find free vars in $(Exp)",locOf(Exp));
      valis Fv
    }
  }

  public implementation freevars[canonAction] => {
    findFree(A,Q) => freeVarsInAct(A,Q,[])
  }

  freeVarsInAct(Ac,Q,Fv) => case Ac in {
    | .doNop(_) => Fv
    | .doSeq(_,L,R) => freeVarsInAct(R,Q,freeVarsInAct(L,Q,Fv))
    | .doLbld(_,_,A) => freeVarsInAct(A,Q,Fv)
    | .doBrk(_,_) => Fv
    | .doValis(_,E) => freeVarsInExp(E,Q,Fv)
    | .doDefn(_,P,E) where Q1 .= dropVars(P,Q) => freeVarsInExp(E,Q1,freeVarsInExp(P,Q1,Fv))
    | .doMatch(_,P,E) where Q1 .= dropVars(P,Q) =>
      freeVarsInExp(E,Q1,freeVarsInExp(P,Q1,Fv))
    | .doAssign(_,L,R) => freeVarsInExp(L,Q,freeVarsInExp(R,Q,Fv))
    | .doTry(_,L,_,H) => foldLeft((Rl,F) => freeVarsInRule(Rl,Q,F),freeVarsInAct(L,Q,Fv), H)
    | .doThrow(_,E) => freeVarsInExp(E,Q,Fv)
    | .doIfThen(Lc,T,L,R) => valof{
      Q1 = Q\condVars(T,[]);
      valis freeVarsInAct(L,Q1,freeVarsInAct(R,Q,freeVarsInCond(T,Q1,Fv)))
    }
    | .doCase(_,G,Cs) =>
      foldLeft((Rl,F)=>freeVarsInRule(Rl,Q,F), freeVarsInExp(G,Q,Fv), Cs)
    | .doWhile(_,C,B) where Q1 .= Q\condVars(C,[]) =>
      freeVarsInAct(B,Q1,freeVarsInCond(C,Q1,Fv))
    | .doLet(_,Dfs,_,A) => valof{
      QD = dropDefs(Dfs,Q);
      valis freeVarsInAct(A,QD,freeVarsInDefs(Dfs,Q,Fv))
    }
    | .doLetRec(_,Dfs,_,A) => valof{
      QD = dropDefs(Dfs,Q);
      valis freeVarsInAct(A,QD,freeVarsInDefs(Dfs,QD,Fv))
    }
    | .doExp(_,C) => freeVarsInExp(C,Q,Fv)
    | _ default => valof{
      reportError("cant find free vars in $(Ac)",locOf(Ac));
      valis Fv
    }
  }

  freeVarsInTuple(Els,Q,Fv) =>
    foldRight((E,F)=>freeVarsInExp(E,Q,F),Fv,Els).
  
  freeVarsInCond:(canon,set[cV],set[cV]) => set[cV].
  freeVarsInCond(.cond(_,T,L,R),Q,Fv) =>
    freeVarsInCond(T,Q,freeVarsInExp(L,Q,freeVarsInExp(R,Q,Fv))).
  freeVarsInCond(.match(_,P,S),Q,Fv) =>
    freeVarsInExp(P,Q,freeVarsInExp(S,Q,Fv)).
  freeVarsInCond(.conj(Lc,L,R),Q,Fv) =>
    freeVarsInCond(L,Q,freeVarsInCond(R,Q,Fv)).
  freeVarsInCond(.disj(Lc,L,R),Q,Fv) =>
    freeVarsInCond(L,Q,freeVarsInCond(R,Q,Fv)).
  freeVarsInCond(.neg(Lc,R),Q,Fv) => freeVarsInCond(R,Q,Fv).
  freeVarsInCond(T,Q,Fv) => freeVarsInExp(T,Q,Fv).

  public freeVarsInRule:all e ~~ freevars[e] |:
    (rule[e],set[cV],set[cV])=>set[cV].
  freeVarsInRule(.rule(_,Ptn,.none,Exp),Q,Fv) => valof{
    Q1 = dropVars(Ptn,Q);
    valis freeVarsInExp(Ptn,Q1,Fv) \/ findFree(Exp,Q1)
  }
  freeVarsInRule(.rule(_,Ptn,.some(Wh),Exp),Q,Fv) =>valof{
    Q1 = dropVars(Ptn,Q);
    valis freeVarsInExp(Ptn,Q1,freeVarsInCond(Wh,Q1,Fv)) \/ findFree(Exp,Q1)
  }

  public freeVarsInGroup:(cons[canonDef],set[cV])=>set[cV].
  freeVarsInGroup(Defs,Q) => let{
    QD = dropDefs(Defs,Q)
  } in foldLeft((D,F)=>freeVarsInDef(D,QD,F),[],Defs).

  public freeVarsInLetRec:(cons[canonDef],canon,set[cV])=>set[cV].
  freeVarsInLetRec(Defs,Bnd,Q) => let{
    QD = dropDefs(Defs,Q)
  } in foldLeft((D,F)=>freeVarsInDef(D,QD,F),
    freeVarsInExp(Bnd,QD,[]),Defs).

  public freeVarsInLetGroup:(cons[canonDef],canon,set[cV])=>set[cV].
  freeVarsInLetGroup(Defs,Bnd,Q) =>let{
    QD = dropDefs(Defs,Q)
  } in foldLeft((D,F)=>freeVarsInDef(D,Q,F),freeVarsInExp(Bnd,QD,[]),Defs).

  public freeVarsInDef:(canonDef,set[cV],set[cV])=>set[cV].
  freeVarsInDef(.funDef(_,_,Rls,_,_),Q,Fv) =>
    foldRight((Rl,F)=>freeVarsInRule(Rl,Q,F),Fv,Rls).
  freeVarsInDef(.varDef(_,_,_,E,_,_),Q,Fv) => freeVarsInExp(E,Q,Fv).
  freeVarsInDef(.implDef(_,_,_,Val,_,_),Q,Fv) => freeVarsInExp(Val,Q,Fv).
  freeVarsInDef(_,_,Fv) default => Fv.

  freeVarsInDefs:(cons[canonDef],set[cV],set[cV])=>set[cV].
  freeVarsInDefs(Defs,Q,Fv)=>foldRight((D,F)=>freeVarsInDef(D,Q,F),Fv,Defs).

  dropVars:(canon,set[cV]) => set[cV].
  dropVars(Ptn,Q) => Q\ ptnVars(Ptn,[],[]).

  dropDefs:(cons[canonDef],set[cV])=>set[cV].
  dropDefs(Defs,Q) => foldRight((D,QQ) => dropDef(D,QQ),Q,Defs).

  dropDef(.funDef(_,Nm,_,_,Tp),Q) => Q\-.cV(Nm,Tp).
  dropDef(.varDef(_,Nm,_,_,_,Tp),Q) => Q\-.cV(Nm,Tp).
  dropDef(.implDef(_,Nm,_,_,_,Tp),Q) => Q\-.cV(Nm,Tp).
  dropDef(_,Q) => Q.

  public condVars:(canon,set[cV]) => set[cV].
  condVars(.cond(_,T,L,R),Vrs) => condVars(L,condVars(T,Vrs))/\ condVars(R,Vrs).
  condVars(.tple(_,Els),Vrs) =>
    foldRight((E,F)=>condVars(E,F),Vrs,Els).
  condVars(.match(_,P,S),Vrs) => ptnVars(P,Vrs,[]).
  condVars(.conj(Lc,L,R),Vrs) => condVars(R,condVars(L,Vrs)).
  condVars(.disj(Lc,L,R),Vrs) => condVars(L,Vrs)/\condVars(R,Vrs).
  condVars(.neg(Lc,R),Vrs) => Vrs.
  condVars(_,Vrs) default => Vrs.
  
  public ptnVars:(canon,set[cV],set[cV]) => set[cV].
  ptnVars(Exp,Q,Fv) => case Exp in {
    | .anon(_,_) => Q
    | .vr(Lc,Nm,Tp) => 
      {? .cV(Nm,Tp) in Q || .cV(Nm,_) in Fv ?} ?? Q || Q\+.cV(Nm,Tp)
    | .intr(_,_) => Q
    | .flt(_,_) => Q
    | .kar(_,_) => Q
    | .strng(_,_) => Q
    | .enm(_,_,_) => Q
    | .svGet(_,E,_) => ptnVars(E,Q,Fv)
    | .svSet(_,E,V) => ptnVars(V,Q,ptnVars(E,Q,Fv))
    | .newSav(_,_) => Q
    | .mtd(_,_,_) => Q
    | .over(_,V,_) => ptnVars(V,Q,Fv)
    | .cond(_,T,L,R) => ptnVars(L,ptnVars(T,Q,Fv),Fv)/\ ptnVars(R,Q,Fv)
    | .apply(_,O,A,_) => ptnTplVars(A,Q,Fv)
    | .tapply(_,O,A,_,_) => ptnTplVars(A,Q,Fv)
    | .tple(_,Els) => ptnTplVars(Els,Q,Fv)
    | .match(_,P,S) => ptnVars(P,Q,Fv)
    | .conj(Lc,L,R) => ptnVars(R,ptnVars(L,Q,Fv),Fv)
    | .disj(Lc,L,R) => ptnVars(L,Q,Fv)/\ptnVars(R,Q,Fv)
    | .neg(Lc,R) => Q
    | .lambda(_,_,_,_) => Q
    | .letExp(_,B,_,E) => Q
    | .letRec(_,B,_,E) => Q
  }

  ptnTplVars:(cons[canon],set[cV],set[cV])=>set[cV].
  ptnTplVars(Els,Q,Fv) => foldRight((E,F)=>ptnVars(E,F,Fv),Q,Els).

  -- Variables that might be introduced in an action
  public actnVars:(canonAction,set[cV]) => set[cV].
  actnVars(Ac,Q) => case Ac in {
    | .doDefn(_,P,_) => ptnVars(P,Q,[])
    | .doMatch(_,P,_) => ptnVars(P,Q,[])
    | .doSeq(_,L,R) => actnVars(R,actnVars(L,Q))
    | _ default => Q
  }
}
