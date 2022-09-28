star.compiler.freevars{
  import star.
  import star.sets.

  import star.compiler.canon.
  import star.compiler.location.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.misc.
  import star.compiler.term.
  import star.compiler.types.

  public freeVarsInExp:(canon,set[cId],set[cId],set[cId]) => set[cId].
  freeVarsInExp(vr(Lc,Nm,Tp),Excl,_,Fv) where {? cId(Nm,_) in Excl ?} => Fv.
  freeVarsInExp(vr(Lc,Nm,Tp),Excl,_,Fv) where {? cId(Nm,_) in Fv ?} => Fv.
  freeVarsInExp(vr(_,Nm,_),_,_,Fv) where _ ^= isEscape(Nm) => Fv.
  freeVarsInExp(vr(_,Nm,_),_,_,Fv) where _ ^= intrinsic(Nm) => Fv.
  freeVarsInExp(vr(Lc,Nm,Tp),_,Q,Fv) => ({? cId(Nm,_) in Q ?} ? Fv\+cId(Nm,Tp) || Fv).
  freeVarsInExp(intr(_,_),_,_,Fv) => Fv.
  freeVarsInExp(flt(_,_),_,_,Fv) => Fv.
  freeVarsInExp(strng(_,_),_,_,Fv) => Fv.
  freeVarsInExp(enm(_,_,_),_,_,Fv) => Fv.
  freeVarsInExp(dot(_,Rc,_,_),Excl,Q,Fv) => freeVarsInExp(Rc,Excl,Q,Fv).
  freeVarsInExp(mtd(_,_,_),_,_,Fv) => Fv.
  freeVarsInExp(over(_,V,_),Excl,Q,Fv) => freeVarsInExp(V,Excl,Q,Fv).
  freeVarsInExp(overaccess(_,V,_,_,_),Excl,Q,Fv) => freeVarsInExp(V,Excl,Q,Fv).
  freeVarsInExp(csexp(_,G,Cs,_),Excl,Q,Fv) =>
    foldLeft((Rl,F)=>freeVarsInRule(Rl,Excl,Q,F),
      freeVarsInExp(G,Excl,Q,Fv),
      Cs).
  freeVarsInExp(whr(_,E,C),Excl,Q,Fv) =>
    freeVarsInExp(C,Excl,Q,freeVarsInExp(E,Excl,Q,Fv)).
  freeVarsInExp(cond(_,T,L,R),Excl,Q,Fv) where Fv1 .= freeVarsInCond(T,Excl,Q,Fv) =>
    freeVarsInExp(L,Excl,Q,freeVarsInExp(R,Excl,Q,Fv1)).
  freeVarsInExp(apply(_,O,A,_),Excl,Q,Fv) =>
    freeVarsInTuple(A,Excl,Q,freeVarsInExp(O,Excl,Q,Fv)).
  freeVarsInExp(tple(_,Els),Excl,Q,Fv) => freeVarsInTuple(Els,Excl,Q,Fv).
  freeVarsInExp(match(_,P,S),Excl,Q,Fv) where Excl1 .= extendExcl(P,Excl,Fv) =>
    freeVarsInExp(S,Excl1,Q,freeVarsInExp(P,Excl1,Q,Fv)).
  freeVarsInExp(conj(Lc,L,R),Excl,Q,Fv) => freeVarsInCond(conj(Lc,L,R),Excl,Q,Fv).
  freeVarsInExp(disj(Lc,L,R),Excl,Q,Fv) => freeVarsInCond(disj(Lc,L,R),Excl,Q,Fv).
  freeVarsInExp(neg(Lc,R),Excl,Q,Fv) => freeVarsInCond(neg(Lc,R),Excl,Q,Fv).
  freeVarsInExp(lambda(_,_,Eqns,_),Excl,Q,Fv) =>
    foldRight((Rl,F)=>freeVarsInRule(Rl,Excl,Q,F),Fv,Eqns).
  freeVarsInExp(letExp(_,D,_,E),Excl,Q,Fv) => let{
    XX = exclDfs(D,Excl,Fv)
  } in freeVarsInExp(E,XX,Q,freeVarsInDefs(D,Excl,Q,Fv)).
  freeVarsInExp(letRec(_,D,_,E),Excl,Q,Fv) => let{
    XX = exclDfs(D,Excl,Fv)
  } in freeVarsInExp(E,XX,Q,freeVarsInDefs(D,XX,Q,Fv)).

  freeVarsInTuple(Els,Excl,Q,Fv) =>
    foldRight((E,F)=>freeVarsInExp(E,Excl,Q,F),Fv,Els).
  
  freeVarsInCond:(canon,set[cId],set[cId],set[cId]) => set[cId].
  freeVarsInCond(cond(_,T,L,R),Excl,Q,Fv) =>
    freeVarsInCond(T,Excl,Q,freeVarsInExp(L,Excl,Q,freeVarsInExp(R,Excl,Q,Fv))).
  freeVarsInCond(match(_,P,S),Excl,Q,Fv) =>
    freeVarsInExp(P,Excl,Q,freeVarsInExp(S,Excl,Q,Fv)).
  freeVarsInCond(conj(Lc,L,R),Excl,Q,Fv) =>
    freeVarsInCond(L,Excl,Q,freeVarsInCond(R,Excl,Q,Fv)).
  freeVarsInCond(disj(Lc,L,R),Excl,Q,Fv) =>
    freeVarsInCond(L,Excl,Q,freeVarsInCond(R,Excl,Q,Fv)).
  freeVarsInCond(neg(Lc,R),Excl,Q,Fv) => freeVarsInCond(R,Excl,Q,Fv).
  freeVarsInCond(T,Excl,Q,Fv) => freeVarsInExp(T,Excl,Q,Fv).

  freeVarsInRule(rule(_,Ptn,.none,Exp),Excl,Q,Fv) =>
    freeVarsInExp(Ptn,Excl,Q,freeVarsInExp(Exp,Excl,Q,Fv)).
  freeVarsInRule(rule(_,Ptn,some(Wh),Exp),Excl,Q,Fv) =>
    freeVarsInExp(Ptn,Excl,Q,freeVarsInExp(Exp,Excl,Q,freeVarsInCond(Wh,Excl,Q,Fv))).

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
  freeVarsInDef(varDef(_,_,_,E,_,_),Excl,Q,Fv) =>
    freeVarsInExp(E,Excl,Q,Fv).
  freeVarsInDef(implDef(_,_,_,Val,_,_),Excl,Q,Fv) =>
    freeVarsInExp(Val,Excl,Q,Fv).
  freeVarsInDef(_,_,_,Fv) default => Fv.

  freeVarsInDefs:(cons[canonDef],set[cId],set[cId],set[cId])=>set[cId].
  freeVarsInDefs(Defs,Excl,Q,Fv)=>foldRight((D,F)=>freeVarsInDef(D,Excl,Q,F),Fv,Defs).

  extendExcl:(canon,set[cId],set[cId]) => set[cId].
  extendExcl(P,Excl,Fv) => ptnVars(P,Excl,Fv).

  exclDfs:(cons[canonDef],set[cId],set[cId])=>set[cId].
  exclDfs(Defs,Excl,Fv) => foldRight((D,Ex)=>exclDf(D,Ex,Fv),Excl,Defs).

  exclDf(varDef(Lc,Nm,_,Val,_,Tp),Excl,Fv) => Excl\+cId(Nm,Tp).
  exclDf(implDef(Lc,Nm,FullNm,Val,Cx,Tp),Excl,Fv) =>
    exclDf(varDef(Lc,Nm,FullNm,Val,Cx,Tp),Excl,Fv).
  exclDf(_,Excl,_) => Excl.

  public glVars:(canon,set[cId]) => set[cId].
  glVars(whr(_,E,C),Vrs) =>
    glVars(C,glVars(E,Vrs)).
  glVars(cond(_,T,L,R),Vrs) => glVars(L,glVars(T,Vrs))/\ glVars(R,Vrs).
  glVars(tple(_,Els),Vrs) =>
    foldRight((E,F)=>glVars(E,F),Vrs,Els).
  glVars(match(_,P,S),Vrs) => ptnVars(P,Vrs,[]).
  glVars(conj(Lc,L,R),Vrs) => glVars(R,glVars(L,Vrs)).
  glVars(disj(Lc,L,R),Vrs) => glVars(L,Vrs)/\glVars(R,Vrs).
  glVars(neg(Lc,R),Vrs) => Vrs.
  glVars(_,Vrs) default => Vrs.
  

  public ptnVars:(canon,set[cId],set[cId]) => set[cId].
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) where {? cId(Nm,Tp) in Excl ?} => Excl.
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) where {? cId(Nm,_) in Fv ?} => Excl.
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) => Excl\+cId(Nm,Tp).
  ptnVars(intr(_,_),Excl,_) => Excl.
  ptnVars(flt(_,_),Excl,_) => Excl.
  ptnVars(strng(_,_),Excl,_) => Excl.
  ptnVars(enm(_,_,_),Excl,Fv) => Excl.
  ptnVars(dot(_,Rc,_,_),Excl,Fv) => Excl.
  ptnVars(mtd(_,_,_),Excl,Fv) => Excl.
  ptnVars(over(_,V,_),Excl,Fv) => ptnVars(V,Excl,Fv).
  ptnVars(overaccess(_,V,_,_,_),Excl,Fv) => ptnVars(V,Excl,Fv).
  ptnVars(whr(_,E,C),Excl,Fv) =>
    glVars(C,ptnVars(E,Excl,Fv)).
  ptnVars(cond(_,T,L,R),Excl,Fv) => ptnVars(L,ptnVars(T,Excl,Fv),Fv)/\
    ptnVars(R,Excl,Fv).
  ptnVars(apply(_,O,A,_),Excl,Fv) => ptnTplVars(A,Excl,Fv).
  ptnVars(tple(_,Els),Excl,Fv) => ptnTplVars(Els,Excl,Fv).
  ptnVars(match(_,P,S),Excl,Fv) => 
    ptnVars(S,ptnVars(P,Excl,Fv),Fv).
  ptnVars(conj(Lc,L,R),Excl,Fv) => ptnVars(R,ptnVars(L,Excl,Fv),Fv).
  ptnVars(disj(Lc,L,R),Excl,Fv) => ptnVars(L,Excl,Fv)/\ptnVars(R,Excl,Fv).
  ptnVars(neg(Lc,R),Excl,Fv) => Excl.
  ptnVars(lambda(_,_,Eqns,_),Excl,Fv) => Excl.
  ptnVars(letExp(_,B,_,E),Excl,Fv) => Excl.
  ptnVars(letRec(_,B,_,E),Excl,Fv) => Excl.

  ptnTplVars(Els,Excl,Fv) => 
    foldRight((E,F)=>ptnVars(E,F,Fv),Excl,Els).
  
}
