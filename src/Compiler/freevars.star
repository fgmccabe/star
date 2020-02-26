star.compiler.freevars{
  import star.
  import star.sets.

  import star.compiler.canon.
  import star.compiler.location.
  import star.compiler.core.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.types.

  public freeVarsInTerm:(canon,set[crVar],set[crVar]) => set[crVar].
  freeVarsInTerm(vr(Lc,Nm,Tp),Excl,Fv) where crId(Nm,_) in Excl => Fv.
  freeVarsInTerm(vr(Lc,Nm,Tp),Excl,Fv) where crId(Nm,_) in Fv => Fv.
  freeVarsInTerm(vr(_,Nm,_),_,Fv) where isEscape(Nm) => Fv.
  freeVarsInTerm(vr(_,Nm,_),_,Fv) where _ ^= intrinsic(Nm) => Fv.
  freeVarsInTerm(vr(Lc,Nm,Tp),_,Fv) => _addMem(crId(Nm,Tp),Fv).
  freeVarsInTerm(intr(_,_),_,Fv) => Fv.
  freeVarsInTerm(flot(_,_),_,Fv) => Fv.
  freeVarsInTerm(strng(_,_),_,Fv) => Fv.
  freeVarsInTerm(enm(_,_,_),_,Fv) => Fv.
  freeVarsInTerm(dot(_,Rc,_,_),Excl,Fv) => freeVarsInTerm(Rc,Excl,Fv).
  freeVarsInTerm(mtd(_,_,_),_,Fv) => Fv.
  freeVarsInTerm(over(_,V,_,_),Excl,Fv) => freeVarsInTerm(V,Excl,Fv).
  freeVarsInTerm(act(_,A),Excl,Fv) => freeVarsInAction(A,Excl,Fv).
  freeVarsInTerm(whr(_,E,C),Excl,Fv) =>
    freeVarsInTerm(C,Excl,freeVarsInTerm(E,Excl,Fv)).
  freeVarsInTerm(cond(_,T,L,R),Excl,Fv) where (Excl1,Fv1) .= freeVarsInCond(T,Excl,Fv) =>
    freeVarsInTerm(L,Excl1,freeVarsInTerm(R,Excl,Fv1)).
  freeVarsInTerm(abstraction(_,B,C,_),Excl,Fv) where
      (Excl1,Fv1) .= freeVarsInCond(C,Excl,Fv) =>
    freeVarsInTerm(B,Excl1,Fv1).
  freeVarsInTerm(apply(_,O,A,_),Excl,Fv) =>
    freeVarsInTerm(A,Excl,freeVarsInTerm(O,Excl,Fv)).
  freeVarsInTerm(tple(_,Els),Excl,Fv) =>
    foldRight((E,F)=>freeVarsInTerm(E,Excl,F),Fv,Els).
  freeVarsInTerm(serch(_,P,S,I),Excl,Fv) where Excl1 .= extendExcl(P,Excl,Fv) =>
    freeVarsInTerm(S,Excl1,freeVarsInTerm(I,Excl1,freeVarsInTerm(P,Excl1,Fv))).
  freeVarsInTerm(match(_,P,S),Excl,Fv) where Excl1 .= extendExcl(P,Excl,Fv) =>
    freeVarsInTerm(S,Excl,freeVarsInTerm(P,Excl1,Fv)).
  freeVarsInTerm(conj(Lc,L,R),Excl,Fv) where
      (_,Fv1) .= freeVarsInCond(conj(Lc,L,R),Excl,Fv) => Fv1.
  freeVarsInTerm(disj(Lc,L,R),Excl,Fv) where
      (_,Fv1) .= freeVarsInCond(disj(Lc,L,R),Excl,Fv) => Fv1.
  freeVarsInTerm(neg(Lc,R),Excl,Fv) where
      (_,Fv1) .= freeVarsInCond(neg(Lc,R),Excl,Fv) => Fv1.
  freeVarsInTerm(lambda(_,Eqns,_),Excl,Fv) =>
    foldRight((Rl,F)=>freeVarsInEqn(Rl,Excl,F),Fv,Eqns).
  freeVarsInTerm(letExp(_,D,E),Excl,Fv) => let{
    XX = exclDfs(D,Excl,Fv)
  } in freeVarsInTerm(E,XX,freeVarsInDefs(D,XX,Fv)).
  freeVarsInTerm(record(Lc,Pth,Fields,Tp),Excl,Fv) =>
    foldRight(((_,V),F)=>freeVarsInTerm(V,Excl,F),Fv,Fields).

  freeVarsInCond:(canon,set[crVar],set[crVar]) => (set[crVar],set[crVar]).
  freeVarsInCond(cond(_,T,L,R),Excl,Fv) where
      (Excl1,Fv1) .= freeVarsInCond(T,Excl,Fv) &&
      (Excl2,Fv2) .= freeVarsInCond(L,Excl1,Fv1) &&
      (_,Fvx) .= freeVarsInCond(R,Excl,Fv2) => (Excl2,Fvx).
  freeVarsInCond(serch(_,P,S,I),Excl,Fv) where Excl1 .= extendExcl(P,Excl,Fv) =>
    (Excl1,freeVarsInTerm(P,Excl1,freeVarsInTerm(S,Excl1,freeVarsInTerm(I,Excl1,Fv)))).
  freeVarsInCond(match(_,P,S),Excl,Fv) where Excl1 .= extendExcl(P,Excl,Fv) =>
    (Excl1,freeVarsInTerm(P,Excl1,freeVarsInTerm(S,Excl1,Fv))).
  freeVarsInCond(conj(Lc,L,R),Excl,Fv) where
      (Excl1,Fv1) .= freeVarsInCond(conj(Lc,L,R),Excl,Fv) =>
    freeVarsInCond(R,Excl1,Fv1).
  freeVarsInCond(disj(Lc,L,R),Excl,Fv) where
      (Excl1,Fv1) .= freeVarsInCond(L,Excl,Fv) &&
      (Excl2,Fv2) .= freeVarsInCond(R,Excl,Fv1) => (Excl1/\Excl2,Fv2).
  freeVarsInCond(neg(Lc,R),Excl,Fv) where
      (_,Fv1) .= freeVarsInCond(R,Excl,Fv) => (Excl,Fv1).
  freeVarsInCond(T,Excl,Fv) => (Excl,freeVarsInTerm(T,Excl,Fv)).

  freeVarsInEqn(eqn(_,Ptn,none,Exp),Excl,Fv) where
      Excl1 .= extendExcl(Ptn,Excl,Fv) =>
    freeVarsInTerm(Ptn,Excl1,freeVarsInTerm(Exp,Excl1,Fv)).
  freeVarsInEqn(eqn(_,Ptn,some(Wh),Exp),Excl,Fv) where
      Excl1 .= extendExcl(Wh,extendExcl(Ptn,Excl,Fv),Fv) =>
    freeVarsInTerm(Ptn,Excl1,freeVarsInTerm(Exp,Excl1,Fv)).

  public freeVarsInGroup:(list[canonDef],set[crVar],set[crVar])=>set[crVar].
  freeVarsInGroup(Defs,Excl,Fv) => let{
    Excl1 = exclDfs(Defs,Excl,Fv)
  } in foldRight((D,F)=>freeVarsInDef(D,Excl1,F),Fv,Defs).

  freeVarsInDef(varDef(_,_,_,E,_,_),Excl,Fv) =>
    freeVarsInTerm(E,Excl,Fv).
  freeVarsInDef(_,_,Fv) default => Fv.

  freeVarsInDefs:(list[canonDef],set[crVar],set[crVar])=>set[crVar].
  freeVarsInDefs(Defs,Excl,Fv)=>foldRight((D,F)=>freeVarsInDef(D,Excl,F),Fv,Defs).

  freeVarsInAction(noDo(_),_,Fv) => Fv.
  freeVarsInAction(seqnDo(_,L,R),Excl,Fv) =>
    freeVarsInAction(R,Excl,freeVarsInAction(L,Excl,Fv)).
  freeVarsInAction(bindDo(_,L,R,_,_,_),Excl,Fv) where Excl1.=extendExcl(L,Excl,Fv) =>
    freeVarsInTerm(R,Excl1,freeVarsInTerm(L,Excl1,Fv)).
  freeVarsInAction(varDo(_,L,R),Excl,Fv) where Excl1.=extendExcl(L,Excl,Fv) =>
    freeVarsInTerm(R,Excl1,freeVarsInTerm(L,Excl1,Fv)).
  freeVarsInAction(delayDo(_,A,_,_,_),Excl,Fv) =>
    freeVarsInAction(A,Excl,Fv).
  freeVarsInAction(ifThenElseDo(_,T,L,R,_,_,_),Excl,Fv) where
      (Excl1,Fv1) .= freeVarsInCond(T,Excl,Fv) =>
    freeVarsInAction(R,Excl,freeVarsInAction(L,Excl1,Fv1)).
  freeVarsInAction(whileDo(_,T,B,_,_),Excl,Fv) where
      (Excl1,Fv1) .= freeVarsInCond(T,Excl,Fv) =>
    freeVarsInAction(B,Excl1,Fv1).
  freeVarsInAction(forDo(_,T,B,_,_),Excl,Fv) where
      (Excl1,Fv1) .= freeVarsInCond(T,Excl,Fv) =>
    freeVarsInAction(B,Excl1,Fv1).
  freeVarsInAction(tryCatchDo(_,B,C,_,_,_),Excl,Fv) =>
      freeVarsInAction(B,Excl,freeVarsInTerm(C,Excl,Fv)).
  freeVarsInAction(returnDo(_,E,_,_,_),Excl,Fv) =>
    freeVarsInTerm(E,Excl,Fv).
  freeVarsInAction(throwDo(_,E,_,_,_),Excl,Fv) =>
    freeVarsInTerm(E,Excl,Fv).
  freeVarsInAction(simpleDo(_,E,_),Excl,Fv) =>
    freeVarsInTerm(E,Excl,Fv).

  extendExcl:(canon,set[crVar],set[crVar]) => set[crVar].
  extendExcl(P,Excl,Fv) => ptnVars(P,Excl,Fv).

  exclDfs:(list[canonDef],set[crVar],set[crVar])=>set[crVar].
  exclDfs(Defs,Excl,Fv) => foldRight((D,Ex)=>exclDf(D,Ex,Fv),Excl,Defs).

  exclDf(varDef(Lc,Nm,_,_,_,Tp),Excl,Fv) => _addMem(crId(Nm,Tp),Excl).
--  exclDf(varDef(Lc,Nm,_,lambda(_,_,_),_,Tp),Excl,Fv) => _addMem(crId(Nm,Tp),Excl).
--  exclDf(cnsDef(_,Nm,_,Tp),Excl,Fv) => _addMem(crId(Nm,Tp),Excl).
  exclDf(implDef(_,_,Nm,_,Tp),Excl,Fv) => _addMem(crId(Nm,Tp),Excl).
  exclDf(_,Excl,_) => Excl.

  public goalVars:(locn,canon)=>list[canon].
  goalVars(Lc,Cond) => ((ptnVars(Cond,[],[])::list[crVar])//(crId(Nm,Tp))=>vr(Lc,Nm,Tp)).

  ptnVars:(canon,set[crVar],set[crVar]) => set[crVar].
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) where crId(Nm,Tp) in Excl => Excl.
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) where crId(Nm,_) in Fv => Excl.
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) => _addMem(crId(Nm,Tp),Excl).
  ptnVars(intr(_,_),Excl,_) => Excl.
  ptnVars(flot(_,_),Excl,_) => Excl.
  ptnVars(strng(_,_),Excl,_) => Excl.
  ptnVars(enm(_,_,_),Excl,Fv) => Excl.
  ptnVars(dot(_,Rc,_,_),Excl,Fv) => ptnVars(Rc,Excl,Fv).
  ptnVars(mtd(_,_,_),Excl,Fv) => Excl.
  ptnVars(over(_,V,_,_),Excl,Fv) => ptnVars(V,Excl,Fv).
  ptnVars(act(_,A),Excl,Fv) => Excl.
  ptnVars(whr(_,E,C),Excl,Fv) =>
    ptnVars(C,ptnVars(E,Excl,Fv),Fv).
  ptnVars(cond(_,T,L,R),Excl,Fv) => ptnVars(L,ptnVars(T,Excl,Fv),Fv)/\
    ptnVars(R,Excl,Fv).
  ptnVars(abstraction(_,B,C,_),Excl,Fv) => Excl.
  ptnVars(apply(_,O,A,_),Excl,Fv) =>
    ptnVars(A,ptnVars(O,Excl,Fv),Fv).
  ptnVars(tple(_,Els),Excl,Fv) =>
    foldRight((E,F)=>ptnVars(E,F,Fv),Excl,Els).
  ptnVars(serch(_,P,S,I),Excl,Fv) => Excl.
  ptnVars(match(_,P,S),Excl,Fv) => 
    ptnVars(S,ptnVars(P,Excl,Fv),Fv).
  ptnVars(conj(Lc,L,R),Excl,Fv) => ptnVars(R,ptnVars(L,Excl,Fv),Fv).
  ptnVars(disj(Lc,L,R),Excl,Fv) => ptnVars(L,Excl,Fv)/\ptnVars(R,Excl,Fv).
  ptnVars(neg(Lc,R),Excl,Fv) => Excl.
  ptnVars(lambda(_,Eqns,_),Excl,Fv) => Excl.
  ptnVars(letExp(_,B,E),Excl,Fv) => Excl.
  ptnVars(record(Lc,Pth,Defs,Tp),Excl,Fv) => foldRight(((_,P),F)=>ptnVars(P,Excl,F),Fv,Defs).
}
