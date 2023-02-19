star.compiler.freevars{
  import star.
  import star.sets.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.misc.
  import star.compiler.term.
  import star.compiler.types.

  public contract all e ~~ freevars[e] ::= {
    findFree:(e,set[cId]) => set[cId].
  }

  public implementation freevars[canon] => {
    findFree(E,Q) => freeVarsInExp(E,[],Q,[])
  }

  public freeVarsInExp:(canon,set[cId],set[cId],set[cId]) => set[cId].
  freeVarsInExp(Exp,Excl,Q,Fv) => case Exp in {
    .anon(_,_) => Fv.
    .vr(Lc,Nm,Tp) where {? .cId(Nm,_) in Excl ?} => Fv.
    .vr(Lc,Nm,Tp) where {? .cId(Nm,_) in Fv ?} => Fv.
    .vr(_,Nm,_) where _ ?= isEscape(Nm) => Fv.
    .vr(_,Nm,_) where _ ?= intrinsic(Nm) => Fv.
    .vr(Lc,Nm,Tp) => ({? .cId(Nm,_) in Q ?} ?? Fv\+.cId(Nm,Tp) || Fv).
    .intr(_,_) => Fv.
    .bintr(_,_) => Fv.
    .kar(_,_) => Fv.
    .flt(_,_) => Fv.
    .strng(_,_) => Fv.
    .enm(_,_,_) => Fv.
    .dot(_,Rc,_,_) => freeVarsInExp(Rc,Excl,Q,Fv).
    .mtd(_,_,_) => Fv.
    .over(_,V,_) => freeVarsInExp(V,Excl,Q,Fv).
    .csexp(_,G,Cs,_) =>
      foldLeft((Rl,F)=>freeVarsInRule(Rl,freeVarsInExp,Excl,Q,F), freeVarsInExp(G,Excl,Q,Fv), Cs).
    .cond(_,T,L,R) where Fv1 .= freeVarsInCond(T,Excl,Q,Fv) =>
      freeVarsInExp(L,Excl,Q,freeVarsInExp(R,Excl,Q,Fv1)).
    .apply(_,O,A,_) =>
      freeVarsInTuple(A,Excl,Q,freeVarsInExp(O,Excl,Q,Fv)).
    .tple(_,Els) => freeVarsInTuple(Els,Excl,Q,Fv).
    .match(_,P,S) where Excl1 .= extendExcl(P,Excl) =>
      freeVarsInExp(S,Excl1,Q,freeVarsInExp(P,Excl1,Q,Fv)).
    .conj(_,L,R) => freeVarsInCond(Exp,Excl,Q,Fv).
    .disj(_,L,R) => freeVarsInCond(Exp,Excl,Q,Fv).
    .neg(_,R) => freeVarsInCond(Exp,Excl,Q,Fv).
    .trycatch(_,E,T,H,_) where Excl1 .= extendExcl(T,Excl) =>
      freeVarsInExp(T,Excl1,Q,freeVarsInExp(E,Excl1,Q,
	  foldRight((Rl,F)=>freeVarsInRule(Rl,freeVarsInExp,Excl,Q,F),Fv,H))).
    .rais(_,T,E,_) => freeVarsInExp(T,Excl,Q,freeVarsInExp(E,Excl,Q,Fv)).
    .lambda(_,_,Eqns,_) =>
      foldRight((Rl,F)=>freeVarsInRule(Rl,freeVarsInExp,Excl,Q,F),Fv,Eqns).
    .letExp(_,D,_,E) => let{
      XX = exclDfs(D,Excl,Fv)
    } in freeVarsInExp(E,XX,Q,freeVarsInDefs(D,Excl,Q,Fv)).
    .letRec(Lc,D,_,E) => valof{
      XX = exclDfs(D,Excl,Fv);
      valis freeVarsInExp(E,XX,Q,freeVarsInDefs(D,XX,Q,Fv))
    }.
    .vlof(_,A,_) => freeVarsInAct(A,Excl,Q,Fv).
    _ default => valof{
      reportError("cant find free vars in $(Exp)",locOf(Exp));
      valis Fv
    }
  }

  public implementation freevars[canonAction] => {
    findFree(A,Q) => freeVarsInAct(A,[],Q,[])
  }

  freeVarsInAct(Ac,Excl,Q,Fv) => case Ac in {
    .doNop(_) => Fv.
    .doSeq(_,L,R) => freeVarsInAct(R,Excl,Q,freeVarsInAct(L,Excl,Q,Fv)).
    .doLbld(_,_,A) => freeVarsInAct(A,Excl,Q,Fv).
    .doBrk(_,_) => Fv.
    .doValis(_,E) => freeVarsInExp(E,Excl,Q,Fv).
    .doRaise(_,T,E) => freeVarsInExp(T,Excl,Q,freeVarsInExp(E,Excl,Q,Fv)).
    .doDefn(_,P,E) where Excl1 .= extendExcl(P,Excl) =>
      freeVarsInExp(E,Excl1,Q,freeVarsInExp(P,Excl1,Q,Fv)).
    .doMatch(_,P,E) where Excl1 .= extendExcl(P,Excl) =>
      freeVarsInExp(E,Excl1,Q,freeVarsInExp(P,Excl1,Q,Fv)).
    .doAssign(_,L,R) => freeVarsInExp(L,Excl,Q,freeVarsInExp(R,Excl,Q,Fv)).
    .doTryCatch(_,L,T,H) where Excl1 .= extendExcl(T,Excl) =>
      foldLeft((Rl,F)=>freeVarsInRule(Rl,freeVarsInAct,Excl,Q,F),freeVarsInAct(L,Excl1,Q,Fv), H).
    .doIfThen(Lc,T,L,R) => valof{
      Excl1 = condVars(T,Excl);
      valis freeVarsInAct(L,Excl1,Q,
	freeVarsInAct(R,Excl,Q,freeVarsInCond(T,Excl1,Q,Fv)))
    }.
    .doCase(_,G,Cs) =>
      foldLeft((Rl,F)=>freeVarsInRule(Rl,freeVarsInAct,Excl,Q,F), freeVarsInExp(G,Excl,Q,Fv), Cs).
    .doWhile(_,C,B) where Excl1 .= condVars(C,Excl) =>
      freeVarsInAct(B,Excl1,Q,freeVarsInCond(C,Excl1,Q,Fv)).
    .doLet(_,Dfs,_,A) => valof{
      XX = exclDfs(Dfs,Excl,Fv);
      valis freeVarsInAct(A,XX,Q,freeVarsInDefs(Dfs,Excl,Q,Fv))
    }.
    .doLetRec(_,Dfs,_,A) => valof{
      XX = exclDfs(Dfs,Excl,Fv);
      valis freeVarsInAct(A,XX,Q,freeVarsInDefs(Dfs,Excl,Q,Fv))
    }.
    .doCall(_,C) => freeVarsInExp(C,Excl,Q,Fv).
    _ default => valof{
      reportError("cant find free vars in $(Ac)",locOf(Ac));
      valis Fv
    }
  }

  freeVarsInTuple(Els,Excl,Q,Fv) =>
    foldRight((E,F)=>freeVarsInExp(E,Excl,Q,F),Fv,Els).
  
  freeVarsInCond:(canon,set[cId],set[cId],set[cId]) => set[cId].
  freeVarsInCond(.cond(_,T,L,R),Excl,Q,Fv) =>
    freeVarsInCond(T,Excl,Q,freeVarsInExp(L,Excl,Q,freeVarsInExp(R,Excl,Q,Fv))).
  freeVarsInCond(.match(_,P,S),Excl,Q,Fv) =>
    freeVarsInExp(P,Excl,Q,freeVarsInExp(S,Excl,Q,Fv)).
  freeVarsInCond(.conj(Lc,L,R),Excl,Q,Fv) =>
    freeVarsInCond(L,Excl,Q,freeVarsInCond(R,Excl,Q,Fv)).
  freeVarsInCond(.disj(Lc,L,R),Excl,Q,Fv) =>
    freeVarsInCond(L,Excl,Q,freeVarsInCond(R,Excl,Q,Fv)).
  freeVarsInCond(.neg(Lc,R),Excl,Q,Fv) => freeVarsInCond(R,Excl,Q,Fv).
  freeVarsInCond(T,Excl,Q,Fv) => freeVarsInExp(T,Excl,Q,Fv).

  freeVarsInRule:all e ~~ (rule[e],
    (e,set[cId],set[cId],set[cId])=>set[cId],set[cId],set[cId],set[cId])=>set[cId].
  freeVarsInRule(.rule(_,Ptn,.none,Exp),Fn,Excl,Q,Fv) => valof{
    Excl1 = extendExcl(Ptn,Excl);
    valis freeVarsInExp(Ptn,Excl1,Q,Fn(Exp,Excl1,Q,Fv))
  }
  freeVarsInRule(.rule(_,Ptn,.some(Wh),Exp),Fn,Excl,Q,Fv) =>valof{
    Excl1 = extendExcl(Ptn,Excl);
    valis freeVarsInExp(Ptn,Excl1,Q,Fn(Exp,Excl1,Q,freeVarsInCond(Wh,Excl1,Q,Fv)))
  }

  public freeVarsInGroup:(cons[canonDef],set[cId])=>set[cId].
  freeVarsInGroup(Defs,Q) => let{
    Excl1 = exclDfs(Defs,[],[]).
  } in foldLeft((D,F)=>freeVarsInDef(D,Excl1,Q,F),[],Defs).

  public freeVarsInLetRec:(cons[canonDef],canon,set[cId])=>set[cId].
  freeVarsInLetRec(Defs,Bnd,Q) => let{
    Excl1 = exclDfs(Defs,[],[])
  } in foldLeft((D,F)=>freeVarsInDef(D,Excl1,Q,F),
    freeVarsInExp(Bnd,Excl1,Q,[]),Defs).

  public freeVarsInLetGroup:(cons[canonDef],canon,set[cId])=>set[cId].
  freeVarsInLetGroup(Defs,Bnd,Q) =>let{
    Excl1 = exclDfs(Defs,[],[])
  } in foldLeft((D,F)=>freeVarsInDef(D,[],Q,F),freeVarsInExp(Bnd,Excl1,Q,[]),Defs).

  freeVarsInDef:(canonDef,set[cId],set[cId],set[cId])=>set[cId].
  freeVarsInDef(.varDef(_,_,E,_,_),Excl,Q,Fv) =>
    freeVarsInExp(E,Excl,Q,Fv).
  freeVarsInDef(.implDef(_,_,_,Val,_,_),Excl,Q,Fv) =>
    freeVarsInExp(Val,Excl,Q,Fv).
  freeVarsInDef(_,_,_,Fv) default => Fv.

  freeVarsInDefs:(cons[canonDef],set[cId],set[cId],set[cId])=>set[cId].
  freeVarsInDefs(Defs,Excl,Q,Fv)=>foldRight((D,F)=>freeVarsInDef(D,Excl,Q,F),Fv,Defs).

  extendExcl:(canon,set[cId]) => set[cId].
  extendExcl(P,Excl) => ptnVars(P,Excl,[]).

  exclDfs:(cons[canonDef],set[cId],set[cId])=>set[cId].
  exclDfs(Defs,Excl,Fv) => foldRight((D,Ex)=>exclDf(D,Ex,Fv),Excl,Defs).

  exclDf(.varDef(Lc,Nm,Val,_,Tp),Excl,Fv) => Excl\+.cId(Nm,Tp).
  exclDf(.implDef(Lc,Nm,FullNm,Val,Cx,Tp),Excl,Fv) => Excl\+.cId(Nm,Tp).
  exclDf(_,Excl,_) => Excl.

  public condVars:(canon,set[cId]) => set[cId].
  condVars(.cond(_,T,L,R),Vrs) => condVars(L,condVars(T,Vrs))/\ condVars(R,Vrs).
  condVars(.tple(_,Els),Vrs) =>
    foldRight((E,F)=>condVars(E,F),Vrs,Els).
  condVars(.match(_,P,S),Vrs) => ptnVars(P,Vrs,[]).
  condVars(.conj(Lc,L,R),Vrs) => condVars(R,condVars(L,Vrs)).
  condVars(.disj(Lc,L,R),Vrs) => condVars(L,Vrs)/\condVars(R,Vrs).
  condVars(.neg(Lc,R),Vrs) => Vrs.
  condVars(_,Vrs) default => Vrs.
  
  public ptnVars:(canon,set[cId],set[cId]) => set[cId].
  ptnVars(Exp,Q,Fv) => case Exp in {
    .anon(_,_) => Q.
    .vr(Lc,Nm,Tp) => 
      {? .cId(Nm,Tp) in Q || .cId(Nm,_) in Fv ?} ?? Q || Q\+.cId(Nm,Tp).
    .intr(_,_) => Q.
    .flt(_,_) => Q.
    .kar(_,_) => Q.
    .strng(_,_) => Q.
    .enm(_,_,_) => Q.
    .dot(_,Rc,_,_) => ptnVars(Rc,Q,Fv).
    .mtd(_,_,_) => Q.
    .over(_,V,_) => ptnVars(V,Q,Fv).
    .cond(_,T,L,R) => ptnVars(L,ptnVars(T,Q,Fv),Fv)/\ ptnVars(R,Q,Fv).
    .apply(_,O,A,_) => ptnTplVars(A,Q,Fv).
    .tple(_,Els) => ptnTplVars(Els,Q,Fv).
    .match(_,P,S) => ptnVars(P,Q,Fv).
    .conj(Lc,L,R) => ptnVars(R,ptnVars(L,Q,Fv),Fv).
    .disj(Lc,L,R) => ptnVars(L,Q,Fv)/\ptnVars(R,Q,Fv).
    .neg(Lc,R) => Q.
    .lambda(_,_,_,_) => Q.
    .letExp(_,B,_,E) => Q.
    .letRec(_,B,_,E) => Q.
  }

  ptnTplVars:(cons[canon],set[cId],set[cId])=>set[cId].
  ptnTplVars(Els,Q,Fv) => foldRight((E,F)=>ptnVars(E,F,Fv),Q,Els).

  -- Variables that might be introduced in an action
  public actnVars:(canonAction,set[cId]) => set[cId].
  actnVars(Ac,Q) => case Ac in {
    .doDefn(_,P,_) => ptnVars(P,Q,[]).
    .doMatch(_,P,_) => ptnVars(P,Q,[]).
    .doSeq(_,L,R) => actnVars(R,actnVars(L,Q)).
    _ default => Q.
  }
}
