star.compiler.freevars{
  import star.
  import star.sets.

  import star.compiler.canon.
  import star.compiler.location.
  import star.compiler.core.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.misc.
  import star.compiler.types.

  public freeVarsInTerm:(canon,set[crVar],set[crVar],set[crVar]) => set[crVar].
  freeVarsInTerm(vr(Lc,Nm,Tp),Excl,_,Fv) where crId(Nm,_) in Excl => Fv.
  freeVarsInTerm(vr(Lc,Nm,Tp),Excl,_,Fv) where crId(Nm,_) in Fv => Fv.
  freeVarsInTerm(vr(_,Nm,_),_,_,Fv) where isEscape(Nm) => Fv.
  freeVarsInTerm(vr(_,Nm,_),_,_,Fv) where _ ^= intrinsic(Nm) => Fv.
  freeVarsInTerm(vr(Lc,Nm,Tp),_,Q,Fv) => (crId(Nm,_) in Q ? _addMem(crId(Nm,Tp),Fv) || Fv).
  freeVarsInTerm(intr(_,_),_,_,Fv) => Fv.
  freeVarsInTerm(flt(_,_),_,_,Fv) => Fv.
  freeVarsInTerm(strng(_,_),_,_,Fv) => Fv.
  freeVarsInTerm(enm(_,_,_),_,_,Fv) => Fv.
  freeVarsInTerm(dot(_,Rc,_,_),Excl,Q,Fv) => freeVarsInTerm(Rc,Excl,Q,Fv).
  freeVarsInTerm(mtd(_,_,_,_),_,_,Fv) => Fv.
  freeVarsInTerm(over(_,V,_,_),Excl,Q,Fv) => freeVarsInTerm(V,Excl,Q,Fv).
  freeVarsInTerm(csexp(_,G,Cs,_),Excl,Q,Fv) =>
    foldLeft((Rl,F)=>freeVarsInEqn(Rl,Excl,Q,F),
      freeVarsInTerm(G,Excl,Q,Fv),
      Cs).
  freeVarsInTerm(whr(_,E,C),Excl,Q,Fv) =>
    freeVarsInTerm(C,Excl,Q,freeVarsInTerm(E,Excl,Q,Fv)).
  freeVarsInTerm(cond(_,T,L,R),Excl,Q,Fv) where Fv1 .= freeVarsInCond(T,Excl,Q,Fv) =>
    freeVarsInTerm(L,Excl,Q,freeVarsInTerm(R,Excl,Q,Fv1)).
  freeVarsInTerm(apply(_,O,A,_),Excl,Q,Fv) =>
    freeVarsInTerm(A,Excl,Q,freeVarsInTerm(O,Excl,Q,Fv)).
  freeVarsInTerm(tple(_,Els),Excl,Q,Fv) =>
    foldRight((E,F)=>freeVarsInTerm(E,Excl,Q,F),Fv,Els).
  freeVarsInTerm(serch(_,P,S,I),Excl,Q,Fv) where Excl1 .= extendExcl(P,Excl,Fv) =>
    freeVarsInTerm(S,Excl1,Q,freeVarsInTerm(I,Excl1,Q,freeVarsInTerm(P,Excl1,Q,Fv))).
  freeVarsInTerm(match(_,P,S),Excl,Q,Fv) where Excl1 .= extendExcl(P,Excl,Fv) =>
    freeVarsInTerm(S,Excl1,Q,freeVarsInTerm(P,Excl1,Q,Fv)).
  freeVarsInTerm(conj(Lc,L,R),Excl,Q,Fv) => freeVarsInCond(conj(Lc,L,R),Excl,Q,Fv).
  freeVarsInTerm(disj(Lc,L,R),Excl,Q,Fv) => freeVarsInCond(disj(Lc,L,R),Excl,Q,Fv).
  freeVarsInTerm(implies(Lc,L,R),Excl,Q,Fv) => freeVarsInCond(implies(Lc,L,R),Excl,Q,Fv).
  freeVarsInTerm(neg(Lc,R),Excl,Q,Fv) => freeVarsInCond(neg(Lc,R),Excl,Q,Fv).
  freeVarsInTerm(lambda(Eqns,_),Excl,Q,Fv) =>
    foldRight((Rl,F)=>freeVarsInEqn(Rl,Excl,Q,F),Fv,Eqns).
  freeVarsInTerm(letExp(_,D,E),Excl,Q,Fv) => let{
    XX = exclDfs(D,Excl,Fv)
  } in freeVarsInTerm(E,XX,Q,freeVarsInDefs(D,XX,Q,Fv)).
  freeVarsInTerm(letRec(_,D,E),Excl,Q,Fv) => let{
    XX = exclDfs(D,Excl,Fv)
  } in freeVarsInTerm(E,XX,Q,freeVarsInDefs(D,XX,Q,Fv)).
  freeVarsInTerm(record(Lc,_,Fields,Tp),Excl,Q,Fv) =>
    foldRight(((_,V),F)=>freeVarsInTerm(V,Excl,Q,F),Fv,Fields).

  freeVarsInCond:(canon,set[crVar],set[crVar],set[crVar]) => set[crVar].
  freeVarsInCond(cond(_,T,L,R),Excl,Q,Fv) =>
    freeVarsInCond(T,Excl,Q,freeVarsInTerm(L,Excl,Q,freeVarsInTerm(R,Excl,Q,Fv))).
  freeVarsInCond(serch(_,P,S,I),Excl,Q,Fv) =>
    freeVarsInTerm(P,Excl,Q,freeVarsInTerm(S,Excl,Q,freeVarsInTerm(I,Excl,Q,Fv))).
  freeVarsInCond(match(_,P,S),Excl,Q,Fv) =>
    freeVarsInTerm(P,Excl,Q,freeVarsInTerm(S,Excl,Q,Fv)).
  freeVarsInCond(conj(Lc,L,R),Excl,Q,Fv) =>
    freeVarsInCond(L,Excl,Q,freeVarsInCond(R,Excl,Q,Fv)).
  freeVarsInCond(disj(Lc,L,R),Excl,Q,Fv) =>
    freeVarsInCond(L,Excl,Q,freeVarsInCond(R,Excl,Q,Fv)).
  freeVarsInCond(implies(Lc,L,R),Excl,Q,Fv) =>
    freeVarsInCond(L,Excl,Q,freeVarsInCond(R,Excl,Q,Fv)).
  freeVarsInCond(neg(Lc,R),Excl,Q,Fv) => freeVarsInCond(R,Excl,Q,Fv).
  freeVarsInCond(T,Excl,Q,Fv) => freeVarsInTerm(T,Excl,Q,Fv).

  freeVarsInEqn(eqn(_,Ptn,.none,Exp),Excl,Q,Fv) =>
    freeVarsInTerm(Ptn,Excl,Q,freeVarsInTerm(Exp,Excl,Q,Fv)).
  freeVarsInEqn(eqn(_,Ptn,some(Wh),Exp),Excl,Q,Fv) =>
    freeVarsInTerm(Ptn,Excl,Q,freeVarsInTerm(Exp,Excl,Q,freeVarsInCond(Wh,Excl,Q,Fv))).

  public freeVarsInGroup:(cons[canonDef],set[crVar])=>set[crVar].
  freeVarsInGroup(Defs,Q) => let{
    Excl1 = exclDfs(Defs,[],[])
  } in foldLeft((D,F)=>freeVarsInDef(D,Excl1,Q,F),[],Defs).

  freeVarsInDef(varDef(_,_,_,E,_,_),Excl,Q,Fv) =>
    freeVarsInTerm(E,Excl,Q,Fv).
  freeVarsInDef(implDef(_,_,_,Val,_,_),Excl,Q,Fv) =>
    freeVarsInTerm(Val,Excl,Q,Fv).
  freeVarsInDef(_,_,_,Fv) default => Fv.

  freeVarsInDefs:(cons[canonDef],set[crVar],set[crVar],set[crVar])=>set[crVar].
  freeVarsInDefs(Defs,Excl,Q,Fv)=>foldRight((D,F)=>freeVarsInDef(D,Excl,Q,F),Fv,Defs).

  extendExcl:(canon,set[crVar],set[crVar]) => set[crVar].
  extendExcl(P,Excl,Fv) => ptnVars(P,Excl,Fv).

  exclDfs:(cons[canonDef],set[crVar],set[crVar])=>set[crVar].
  exclDfs(Defs,Excl,Fv) => foldRight((D,Ex)=>exclDf(D,Ex,Fv),Excl,Defs).

  exclDf(varDef(Lc,Nm,_,_,_,Tp),Excl,Fv) => _addMem(crId(Nm,Tp),Excl).
  exclDf(implDef(_,_,Nm,_,_,Tp),Excl,Fv) => _addMem(crId(Nm,Tp),Excl).
  exclDf(_,Excl,_) => Excl.

  public goalVars:(locn,canon)=>cons[canon].
  goalVars(Lc,Cond) => ((glVars(Cond,[])::cons[crVar])//(crId(Nm,Tp))=>vr(Lc,Nm,Tp)).

  public glVars:(canon,set[crVar]) => set[crVar].
  glVars(whr(_,E,C),Vrs) =>
    glVars(C,glVars(E,Vrs)).
  glVars(cond(_,T,L,R),Vrs) => glVars(L,glVars(T,Vrs))/\ glVars(R,Vrs).
  glVars(tple(_,Els),Vrs) =>
    foldRight((E,F)=>glVars(E,F),Vrs,Els).
  glVars(serch(_,P,S,_),Vrs) => ptnVars(P,Vrs,[]).
  glVars(match(_,P,S),Vrs) => ptnVars(P,Vrs,[]).
  glVars(conj(Lc,L,R),Vrs) => glVars(R,glVars(L,Vrs)).
  glVars(disj(Lc,L,R),Vrs) => glVars(L,Vrs)/\glVars(R,Vrs).
  glVars(neg(Lc,R),Vrs) => Vrs.
  glVars(_,Vrs) default => Vrs.
  

  public ptnVars:(canon,set[crVar],set[crVar]) => set[crVar].
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) where crId(Nm,Tp) in Excl => Excl.
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) where crId(Nm,_) in Fv => Excl.
  ptnVars(vr(Lc,Nm,Tp),Excl,Fv) => _addMem(crId(Nm,Tp),Excl).
  ptnVars(intr(_,_),Excl,_) => Excl.
  ptnVars(flt(_,_),Excl,_) => Excl.
  ptnVars(strng(_,_),Excl,_) => Excl.
  ptnVars(enm(_,_,_),Excl,Fv) => Excl.
  ptnVars(dot(_,Rc,_,_),Excl,Fv) => Excl.
  ptnVars(mtd(_,_,_,_),Excl,Fv) => Excl.
  ptnVars(over(_,V,_,_),Excl,Fv) => ptnVars(V,Excl,Fv).
  ptnVars(whr(_,E,C),Excl,Fv) =>
    glVars(C,ptnVars(E,Excl,Fv)).
  ptnVars(cond(_,T,L,R),Excl,Fv) => ptnVars(L,ptnVars(T,Excl,Fv),Fv)/\
    ptnVars(R,Excl,Fv).
  ptnVars(apply(_,O,A,_),Excl,Fv) =>
    ptnVars(A,Excl,Fv).
  ptnVars(tple(_,Els),Excl,Fv) =>
    foldRight((E,F)=>ptnVars(E,F,Fv),Excl,Els).
  ptnVars(serch(_,P,S,I),Excl,Fv) => Excl.
  ptnVars(match(_,P,S),Excl,Fv) => 
    ptnVars(S,ptnVars(P,Excl,Fv),Fv).
  ptnVars(conj(Lc,L,R),Excl,Fv) => ptnVars(R,ptnVars(L,Excl,Fv),Fv).
  ptnVars(disj(Lc,L,R),Excl,Fv) => ptnVars(L,Excl,Fv)/\ptnVars(R,Excl,Fv).
  ptnVars(neg(Lc,R),Excl,Fv) => Excl.
  ptnVars(lambda(Eqns,_),Excl,Fv) => Excl.
  ptnVars(letExp(_,B,E),Excl,Fv) => Excl.
  ptnVars(letRec(_,B,E),Excl,Fv) => Excl.
  ptnVars(record(Lc,_,Defs,Tp),Excl,Fv) => foldRight(((_,P),F)=>ptnVars(P,Excl,F),Fv,Defs).
}
