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
  freeVarsInExp(Cn,Excl,Q,Fv) => case Cn in {
    .anon(_,_) => Fv.
    .vr(Lc,Nm,Tp) where {? .cId(Nm,_) in Excl ?} => Fv.
    .vr(Lc,Nm,Tp) where {? .cId(Nm,_) in Fv ?} => Fv.
    .vr(_,Nm,_) where _ ?= isEscape(Nm) => Fv.
    .vr(_,Nm,_) where _ ?= intrinsic(Nm) => Fv.
    .vr(Lc,Nm,Tp) => ({? .cId(Nm,_) in Q ?} ? Fv\+.cId(Nm,Tp) || Fv).
    .intr(_,_) => Fv.
    .bintr(_,_) => Fv.
    .kar(_,_) => Fv.
    .flt(_,_) => Fv.
    .strng(_,_) => Fv.
    .enm(_,_,_) => Fv.
    .dot(_,Rc,_,_) => freeVarsInExp(Rc,Excl,Q,Fv).
    .mtd(_,_,_) => Fv.
    .over(_,V,_) => freeVarsInExp(V,Excl,Q,Fv).
    .overaccess(_,V,_,_,_) => freeVarsInExp(V,Excl,Q,Fv).
    .csexp(_,G,Cs,_) =>
      foldLeft((Rl,F)=>freeVarsInRule(Rl,freeVarsInExp,Excl,Q,F), freeVarsInExp(G,Excl,Q,Fv), Cs).
    .whr(_,E,C) =>
      freeVarsInExp(C,Excl,Q,freeVarsInExp(E,Excl,Q,Fv)).
    .cond(_,T,L,R) where Fv1 .= freeVarsInCond(T,Excl,Q,Fv) =>
      freeVarsInExp(L,Excl,Q,freeVarsInExp(R,Excl,Q,Fv1)).
    .apply(_,O,A,_) =>
      freeVarsInTuple(A,Excl,Q,freeVarsInExp(O,Excl,Q,Fv)).
    .tple(_,Els) => freeVarsInTuple(Els,Excl,Q,Fv).
    .match(_,P,S) where Excl1 .= extendExcl(P,Excl,Fv) =>
      freeVarsInExp(S,Excl1,Q,freeVarsInExp(P,Excl1,Q,Fv)).
    .conj(Lc,L,R) => freeVarsInCond(conj(Lc,L,R),Excl,Q,Fv).
    .disj(Lc,L,R) => freeVarsInCond(disj(Lc,L,R),Excl,Q,Fv).
    .neg(Lc,R) => freeVarsInCond(neg(Lc,R),Excl,Q,Fv).
    .lambda(_,_,Eqns,_) =>
      foldRight((Rl,F)=>freeVarsInRule(Rl,freeVarsInExp,Excl,Q,F),Fv,Eqns).
    .letExp(_,D,_,E) => let{
      XX = exclDfs(D,Excl,Fv)
    } in freeVarsInExp(E,XX,Q,freeVarsInDefs(D,Excl,Q,Fv)).
    .letRec(_,D,_,E) => let{
      XX = exclDfs(D,Excl,Fv)
    } in freeVarsInExp(E,XX,Q,freeVarsInDefs(D,XX,Q,Fv)).
    .vlof(_,A,_) => freeVarsInAct(A,Excl,Q,Fv).
    _ default => valof{
      reportError("cant find free vars in $(Cn)",locOf(Cn));
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
    .doThrow(_,E) => freeVarsInExp(E,Excl,Q,Fv).
    .doDefn(_,P,E) where Excl1 .= extendExcl(P,Excl,Fv) =>
      freeVarsInExp(E,Excl1,Q,freeVarsInExp(P,Excl1,Q,Fv)).
    .doMatch(_,P,E) where Excl1 .= extendExcl(P,Excl,Fv) =>
      freeVarsInExp(E,Excl1,Q,freeVarsInExp(P,Excl1,Q,Fv)).
    .doAssign(_,L,R) => freeVarsInExp(L,Excl,Q,freeVarsInExp(R,Excl,Q,Fv)).
    .doTryCatch(_,L,H) => 
      foldLeft((Rl,F)=>freeVarsInRule(Rl,freeVarsInAct,Excl,Q,F),freeVarsInAct(L,Excl,Q,Fv), H).
    .doIfThen(_,T,L,R) where Fv1 .= freeVarsInCond(T,Excl,Q,Fv) =>
      freeVarsInAct(L,Excl,Q,freeVarsInAct(R,Excl,Q,Fv1)).
    .doCase(_,G,Cs) =>
      foldLeft((Rl,F)=>freeVarsInRule(Rl,freeVarsInAct,Excl,Q,F), freeVarsInExp(G,Excl,Q,Fv), Cs).
    .doWhile(_,C,B) where Excl1 .= extendExcl(C,Excl,Fv) =>
      freeVarsInAct(B,Excl1,Q,freeVarsInCond(C,Excl1,Q,Fv)).
    .doLet(_,Dfs,_,A) => valof{
      XX = exclDfs(Dfs,Excl,Fv);
      valis freeVarsInAct(A,XX,Q,freeVarsInDefs(Dfs,Excl,Q,Fv))
    }.
    .doLetRec(_,Dfs,_,A) => valof{
      XX = exclDfs(Dfs,Excl,Fv);
      valis freeVarsInAct(A,XX,Q,freeVarsInDefs(Dfs,Excl,Q,Fv))
    }.
    .doSuspend(_,Fb,E,H) => 
      foldLeft((Rl,F)=>freeVarsInRule(Rl,freeVarsInAct,Excl,Q,F),
	freeVarsInExp(Fb,Excl,Q,
	  freeVarsInExp(E,Excl,Q,Fv)),H).
    .doResume(_,Fb,E,H) => 
      foldLeft((Rl,F)=>freeVarsInRule(Rl,freeVarsInAct,Excl,Q,F),
	freeVarsInExp(Fb,Excl,Q,
	  freeVarsInExp(E,Excl,Q,Fv)),H).
    .doRetire(_,F,E) => 
      freeVarsInExp(F,Excl,Q,
	freeVarsInExp(E,Excl,Q,Fv)).
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
  freeVarsInRule(.rule(_,Ptn,.none,Exp),Fn,Excl,Q,Fv) =>
    freeVarsInExp(Ptn,Excl,Q,Fn(Exp,Excl,Q,Fv)).
  freeVarsInRule(.rule(_,Ptn,.some(Wh),Exp),Fn,Excl,Q,Fv) =>
    freeVarsInExp(Ptn,Excl,Q,Fn(Exp,Excl,Q,freeVarsInCond(Wh,Excl,Q,Fv))).

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
  freeVarsInDef(.varDef(_,_,_,E,_,_),Excl,Q,Fv) =>
    freeVarsInExp(E,Excl,Q,Fv).
  freeVarsInDef(.implDef(_,_,_,Val,_,_),Excl,Q,Fv) =>
    freeVarsInExp(Val,Excl,Q,Fv).
  freeVarsInDef(_,_,_,Fv) default => Fv.

  freeVarsInDefs:(cons[canonDef],set[cId],set[cId],set[cId])=>set[cId].
  freeVarsInDefs(Defs,Excl,Q,Fv)=>foldRight((D,F)=>freeVarsInDef(D,Excl,Q,F),Fv,Defs).

  extendExcl:(canon,set[cId],set[cId]) => set[cId].
  extendExcl(P,Excl,Fv) => ptnVars(P,Excl,Fv).

  exclDfs:(cons[canonDef],set[cId],set[cId])=>set[cId].
  exclDfs(Defs,Excl,Fv) => foldRight((D,Ex)=>exclDf(D,Ex,Fv),Excl,Defs).

  exclDf(.varDef(Lc,Nm,_,Val,_,Tp),Excl,Fv) => Excl\+cId(Nm,Tp).
  exclDf(.implDef(Lc,Nm,FullNm,Val,Cx,Tp),Excl,Fv) => Excl\+cId(Nm,Tp).
  exclDf(_,Excl,_) => Excl.

  public glVars:(canon,set[cId]) => set[cId].
  glVars(.whr(_,E,C),Vrs) =>
    glVars(C,glVars(E,Vrs)).
  glVars(.cond(_,T,L,R),Vrs) => glVars(L,glVars(T,Vrs))/\ glVars(R,Vrs).
  glVars(.tple(_,Els),Vrs) =>
    foldRight((E,F)=>glVars(E,F),Vrs,Els).
  glVars(.match(_,P,S),Vrs) => ptnVars(P,Vrs,[]).
  glVars(.conj(Lc,L,R),Vrs) => glVars(R,glVars(L,Vrs)).
  glVars(.disj(Lc,L,R),Vrs) => glVars(L,Vrs)/\glVars(R,Vrs).
  glVars(.neg(Lc,R),Vrs) => Vrs.
  glVars(_,Vrs) default => Vrs.
  
  public ptnVars:(canon,set[cId],set[cId]) => set[cId].
  ptnVars(Cn,Excl,Fv) => case Cn in {
    .anon(_,_) => Fv.
    .vr(Lc,Nm,Tp) => ({? .cId(Nm,Tp) in Excl ?} || {? .cId(Nm,_) in Fv ?}) ?
      Excl ||
      Excl\+.cId(Nm,Tp).
    .intr(_,_) => Excl.
    .flt(_,_) => Excl.
    .kar(_,_) => Excl.
    .strng(_,_) => Excl.
    .enm(_,_,_) => Excl.
    .dot(_,Rc,_,_) => ptnVars(Rc,Excl,Fv).
    .mtd(_,_,_) => Excl.
    .over(_,V,_) => ptnVars(V,Excl,Fv).
    .overaccess(_,V,_,_,_) => ptnVars(V,Excl,Fv).
    .whr(_,E,C) => glVars(C,ptnVars(E,Excl,Fv)).
    .cond(_,T,L,R) => ptnVars(L,ptnVars(T,Excl,Fv),Fv)/\ ptnVars(R,Excl,Fv).
    .apply(_,O,A,_) => ptnTplVars(A,Excl,Fv).
    .tple(_,Els) => ptnTplVars(Els,Excl,Fv).
    .match(_,P,S) => ptnVars(S,ptnVars(P,Excl,Fv),Fv).
    .conj(Lc,L,R) => ptnVars(R,ptnVars(L,Excl,Fv),Fv).
    .disj(Lc,L,R) => ptnVars(L,Excl,Fv)/\ptnVars(R,Excl,Fv).
    .neg(Lc,R) => Excl.
    .lambda(_,_,_,_) => Excl.
    .letExp(_,B,_,E) => Excl.
    .letRec(_,B,_,E) => Excl.
  }

  ptnTplVars(Els,Excl,Fv) => 
    foldRight((E,F)=>ptnVars(E,F,Fv),Excl,Els).
}
