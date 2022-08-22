star.compiler.inline{
  import star.
  import star.sort.

  import star.compiler.term.
  import star.compiler.freevars.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.

  public simplifyDefs:(cons[cDefn]) => cons[cDefn].
  simplifyDefs(Dfs) where Prog .= foldLeft(pickupDefn,[],Dfs) => (Dfs//(D)=>simplifyDefn(D,Prog)).

  pickupDefn:(cDefn,map[string,cDefn])=>map[string,cDefn].
  pickupDefn(fnDef(Lc,Nm,Tp,Args,Val),Map) => Map[Nm->fnDef(Lc,Nm,Tp,Args,Val)].
  pickupDefn(vrDef(Lc,Nm,Tp,Val),Map) => Map[Nm->vrDef(Lc,Nm,Tp,Val)].
  pickupDefn(tpDef(_,_,_,_),Map) => Map.
  pickupDefn(lblDef(_,_,_,_),Map) => Map.

  simplifyDefn:(cDefn,map[string,cDefn])=>cDefn.
  simplifyDefn(fnDef(Lc,Nm,Tp,Args,Val),Prog) =>
    fnDef(Lc,Nm,Tp,Args,simplifyExp(Val,Prog,3)).
  simplifyDefn(vrDef(Lc,Nm,Tp,Val),Prog) =>
    vrDef(Lc,Nm,Tp,simplifyExp(Val,Prog,3)).
  simplifyDefn(D,_) default => D.

  -- There are three possibilities of a match ...
  match[e] ::= .noMatch | .insufficient | matching(e).

  -- ptnMatch tries to match an actual value with a pattern
  ptnMatch:(cExp,cExp,map[string,cExp]) => match[map[string,cExp]].
  ptnMatch(E,cVar(Lc,cId(V,_)),Env) =>
    (T^=Env[V] ? ptnMatch(E,T,Env) || matching(Env[V->E])).
  ptnMatch(cVar(_,V1),_,_) => .insufficient.  -- variables on left only match vars on right
  ptnMatch(cInt(_,Ix),cInt(_,Ix),Env) => matching(Env).
  ptnMatch(cBig(_,Bx),cBig(_,Bx),Env) => matching(Env).
  ptnMatch(cFloat(_,Dx),cFloat(_,Dx),Env) => matching(Env).
  ptnMatch(cChar(_,Cx),cChar(_,Cx),Env) => matching(Env).
  ptnMatch(cString(_,Sx),cString(_,Sx),Env) => matching(Env).
  ptnMatch(cTerm(_,N,A1,_),cTerm(_,N,A2,_),Env) => ptnMatchArgs(A1,A2,Env).
  ptnMatch(cVoid(_,_),_,_) => .insufficient.  -- void on left does not match anything
  ptnMatch(_,cVoid(_,_),_) => .insufficient.  -- void on right does not match anything
  ptnMatch(_,_,_) default => .noMatch.

  ptnMatchArgs([],[],Env) => matching(Env).
  ptnMatchArgs([E1,..L1],[E2,..L2],Env) => case ptnMatch(E1,E2,Env) in {
    .noMatch => .noMatch.
    .insufficient => .insufficient.
    .matching(Ev) => ptnMatchArgs(L1,L2,Ev)
  }
  ptnMatchArgs(_,_,_) default => .noMatch.

  contract all e ~~ simplify[e] ::= {
    simplify:(e,map[string,cDefn],integer) => e.
  }

  implementation simplify[cExp] => {
    simplify(E,Map,Dp) => simplifyExp(E,Map,Dp)
  }

  simplifyExp:(cExp,map[string,cDefn],integer) => cExp.
  simplifyExp(E,P,D) => (D>0 ? simExp(E,P,D-1) || E).
  
  simExp(cVar(Lc,V),Map,Depth) => inlineVar(Lc,V,Map,Depth).
  simExp(cInt(Lc,Ix),_,_) => cInt(Lc,Ix).
  simExp(cBig(Lc,Bx),_,_) => cBig(Lc,Bx).
  simExp(cChar(Lc,Ix),_,_) => cChar(Lc,Ix).
  simExp(cFloat(Lc,Dx),_,_) => cFloat(Lc,Dx).
  simExp(cString(Lc,Sx),_,_) => cString(Lc,Sx).
  simExp(cVoid(Lc,Tp),_,_) => cVoid(Lc,Tp).
  simExp(cTerm(Lc,Fn,Args,Tp),Prog,Depth) =>
    cTerm(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp).
  simExp(cCall(Lc,Fn,Args,Tp),Prog,Depth) =>
    inlineCall(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Prog,Depth).
  simExp(cECall(Lc,Fn,Args,Tp),Prog,Depth) =>
    inlineECall(Lc,Fn,Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Depth).
  simExp(cOCall(Lc,Op,Args,Tp),Prog,Depth) =>
    inlineOCall(Lc,simplifyExp(Op,Prog,Depth),
      Args//(A)=>simplifyExp(A,Prog,Depth),Tp,Prog,Depth).
  simExp(cNth(Lc,T,Ix,Tp),Prog,Depth) =>
    inlineTplOff(Lc,simplifyExp(T,Prog,Depth),Ix,Tp).
  simExp(cSetNth(Lc,T,Ix,Vl),Prog,Depth) =>
    applyTplUpdate(Lc,simplifyExp(T,Prog,Depth),Ix,simplifyExp(Vl,Prog,Depth)).
  simExp(cSeq(Lc,L,R),Prog,Depth) =>
    cSeq(Lc,simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simExp(cCnj(Lc,L,R),Prog,Depth) =>
    applyCnj(Lc,simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simExp(cDsj(Lc,L,R),Prog,Depth) =>
    applyDsj(Lc,simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simExp(cNeg(Lc,R),Prog,Depth) =>
    applyNeg(Lc,simplifyExp(R,Prog,Depth)).
  simExp(cCnd(Lc,T,L,R),Prog,Depth) =>
    applyCnd(Lc,simplifyExp(T,Prog,Depth),
      simplifyExp(L,Prog,Depth),simplifyExp(R,Prog,Depth)).
  simExp(cLtt(Lc,Vr,Bnd,Exp),Prog,Depth) =>
    inlineLtt(Lc,Vr,simplifyExp(Bnd,Prog,Depth),Exp,Prog,Depth).
  simExp(cUnpack(Lc,Gov,Cases,Tp),Prog,Depth) =>
    inlineUnpack(Lc,simplifyExp(Gov,Prog,Depth),Cases,Prog,Depth).
  simExp(cCase(Lc,Gov,Cases,Deflt,Tp),Prog,Depth) =>
    inlineCase(Lc,simplifyExp(Gov,Prog,Depth),Cases,
      simplifyExp(Deflt,Prog,Depth),Prog,Depth).
  simExp(cWhere(Lc,Ptn,Exp),Prog,Depth) =>
    applyWhere(Lc,Ptn,simplifyExp(Exp,Prog,Depth)).
  simExp(cMatch(Lc,Ptn,Exp),Prog,Depth) =>
    applyMatch(Lc,Ptn,simplifyExp(Exp,Prog,Depth)).
  simExp(cVarNmes(Lc,Vrs,Exp),Prog,Depth) =>
    cVarNmes(Lc,Vrs,simplifyExp(Exp,Prog,Depth)).
  simExp(cAbort(Lc,Txt,Tp),_,_) => cAbort(Lc,Txt,Tp).
  simExp(cTask(Lc,Exp,Tp),Prog,Depth) =>
    cTask(Lc,simplifyExp(Exp,Prog,Depth),Tp).
  simExp(cSusp(Lc,Tsk,Evt,Tp),Prog,Depth) =>
    cSusp(Lc,simplifyExp(Tsk,Prog,Depth),simplifyExp(Evt,Prog,Depth),Tp).
  simExp(cResume(Lc,Tsk,Evt,Tp),Prog,Depth) =>
    cResume(Lc,simplifyExp(Tsk,Prog,Depth),simplifyExp(Evt,Prog,Depth),Tp).
  simExp(cTry(Lc,Exp,H,Tp),Prog,Depth) =>
    cTry(Lc,simplifyExp(Exp,Prog,Depth),simplifyExp(H,Prog,Depth),Tp).
  simExp(cValof(Lc,Act,Tp),Prog,Depth) =>
    cValof(Lc,simplifyAct(Act,Prog,Depth),Tp).

  implementation simplify[aAction] => {
    simplify(A,Map,Dp) => simplifyAct(A,Map,Dp)
  }

  simplifyAct:(aAction,map[string,cDefn],integer) => aAction.
  simplifyAct(A,P,D) => (D>0 ? simAct(A,P,D-1) || A).

  simAct(aNop(Lc),_,_) => aNop(Lc).
  simAct(aSeq(Lc,A1,A2),Map,Depth) =>
    dropNops(Lc,simplifyAct(A1,Map,Depth),simplifyAct(A2,Map,Depth)).
  simAct(aLbld(Lc,Lb,A),Map,Depth) => valof{
    AA = simplifyAct(A,Map,Depth);
    if lblUsed(AA,Lb) then
      valis aLbld(Lc,Lb,AA)
    else
    valis AA
  }
  simAct(aBreak(Lc,Lb),_,_) => aBreak(Lc,Lb).
  simAct(aValis(Lc,E),Map,Depth) => aValis(Lc,simplifyExp(E,Map,Depth)).
  simAct(aThrow(Lc,E),Map,Depth) => aThrow(Lc,simplifyExp(E,Map,Depth)).
  simAct(aPerf(Lc,E),Map,Depth) => aPerf(Lc,simplifyExp(E,Map,Depth)).
  simAct(aDefn(Lc,V,E),Map,Depth) =>
    aDefn(Lc,simplifyExp(V,Map,Depth),simplifyExp(E,Map,Depth)).
  simAct(aAsgn(Lc,V,E),Map,Depth) =>
    aAsgn(Lc,simplifyExp(V,Map,Depth),simplifyExp(E,Map,Depth)).
  simAct(aCase(Lc,Gov,Cases,Deflt),Prog,Depth) =>
    inlineCase(Lc,simplifyExp(Gov,Prog,Depth),Cases,
      simplifyAct(Deflt,Prog,Depth),Prog,Depth).
  simAct(aUnpack(Lc,Gov,Cases),Prog,Depth) =>
    inlineUnpack(Lc,simplifyExp(Gov,Prog,Depth),Cases,Prog,Depth).
  simAct(aIftte(Lc,T,L,R),Prog,Depth) =>
    applyCnd(Lc,simplifyExp(T,Prog,Depth),
      simplifyAct(L,Prog,Depth),simplifyAct(R,Prog,Depth)).
  simAct(aWhile(Lc,T,A),Prog,Depth) =>
    aWhile(Lc,simplifyExp(T,Prog,Depth),
      simplifyAct(A,Prog,Depth)).
  simAct(aRetire(Lc,T,E),Prog,Depth) =>
    aRetire(Lc,simplifyExp(T,Prog,Depth),
      simplifyExp(E,Prog,Depth)).
  simAct(aTry(Lc,T,H),Prog,Depth) =>
    aTry(Lc,simplifyAct(T,Prog,Depth),simplifyAct(H,Prog,Depth)).
  simAct(aLtt(Lc,Vr,Bnd,A),Prog,Depth) =>
    inlineLtt(Lc,Vr,simplifyExp(Bnd,Prog,Depth),A,Prog,Depth).
  simAct(aVarNmes(Lc,Vrs,X),Prog,Depth) =>
    aVarNmes(Lc,Vrs,simplifyAct(X,Prog,Depth)).
  simAct(aAbort(Lc,Txt),_,_) => aAbort(Lc,Txt).

  dropNops(_,aNop(_),A) => A.
  dropNops(_,A,aNop(_)) => A.
  dropNops(Lc,L,R) => aSeq(Lc,L,R).
  
  inlineVar(Lc,cId(Id,Tp),Map,_Depth) where
      vrDef(_,_,_,Vl) ^= Map[Id] && isGround(Vl) => Vl.
  inlineVar(Lc,V,_,_) => cVar(Lc,V).

  applyWhere(Lc,Ptn,cTerm(_,"star.core#true",[],_)) => Ptn.
  applyWhere(Lc,Ptn,Exp) => cWhere(Lc,Ptn,Exp).

  applyCnj(_,cTerm(_,"star.core#true",[],_),R) => R.
  applyCnj(_,cTerm(Lc,"star.core#false",[],Tp),R) => cTerm(Lc,"star.core#false",[],Tp).
  applyCnj(Lc,L,R) => cCnj(Lc,L,R).

  applyDsj(_,cTerm(_,"star.core#false",[],_),R) => R.
  applyDsj(_,cTerm(Lc,"star.core#true",[],Tp),R) => 
    cTerm(Lc,"star.core#false",[],Tp).
  applyDsj(Lc,L,R) => cDsj(Lc,L,R).

  applyNeg(_,cTerm(Lc,"star.core#false",[],Tp)) => cTerm(Lc,"star.core#true",[],Tp).
  applyNeg(_,cTerm(Lc,"star.core#true",[],Tp)) => cTerm(Lc,"star.core#false",[],Tp).
  applyNeg(Lc,Inner) => cNeg(Lc,Inner).

  applyCnd:all e ~~ reform[e] |: (option[locn],cExp,e,e) => e.
  applyCnd(_,cTerm(_,"star.core#false",[],_),L,R) => R.
  applyCnd(_,cTerm(Lc,"star.core#true",[],Tp),L,_) => L.
  applyCnd(Lc,T,L,R) => mkCond(Lc,T,L,R).

  inlineTplOff(_,cTerm(_,_,Els,_),Ix,Tp) where E^=Els[Ix] => E.
  inlineTplOff(Lc,T,Ix,Tp) default => cNth(Lc,T,Ix,Tp).
  
  applyTplUpdate(_,cTerm(Lc,Nm,Args,Tp),Ix,E) =>
    cTerm(Lc,Nm,Args[Ix->E],Tp).
  applyTplUpdate(Lc,T,Ix,E) =>
    cSetNth(Lc,T,Ix,E).

  applyMatch(Lc,Ptn,Exp) => cMatch(Lc,Ptn,Exp).

  inlineCase:all e ~~ rewrite[e], reform[e], simplify[e] |:
    (option[locn],cExp,cons[cCase[e]],e,map[string,cDefn],integer) => e.
  inlineCase(Lc,Gov,Cases,Deflt,Prog,Depth) where 
      matching(Exp) .= matchingCase(Gov,Cases,Prog,Depth) => Exp.
  inlineCase(Lc,Gov,Cases,Deflt,Prog,Depth) =>
    mkCase(Lc,Gov,Cases,Deflt).

  inlineUnpack:all e ~~ rewrite[e], reform[e], simplify[e] |:
    (option[locn],cExp,cons[cCase[e]],map[string,cDefn],integer) => e.
  inlineUnpack(Lc,Gov,Cases,Prog,Depth) where
      matching(Exp) .= matchingCase(Gov,Cases,Prog,Depth) => Exp.
  inlineUnpack(Lc,Gov,Cases,_,_) => mkUnpack(Lc,Gov,Cases).

  matchingCase:all e ~~ rewrite[e], reform[e], simplify[e] |:
    (cExp,cons[cCase[e]],map[string,cDefn],integer) => match[e].
  matchingCase(_,[],_,_) => .noMatch.
  matchingCase(Gov,[C,..Cs],Prog,Depth) => case candidate(Gov,C) in {
    .insufficient => .insufficient.
    .noMatch => matchingCase(Gov,Cs,Prog,Depth).
    matching(Rep) => matching(simplify(Rep,Prog,Depth))
  }

  candidate:all e ~~ rewrite[e] |: (cExp,cCase[e]) => match[e].
  candidate(E,(_,Ptn,Rep)) => case ptnMatch(E,Ptn,[]) in {
    matching(Theta) => matching(rewrite(Rep,Theta)).
    .noMatch => .noMatch.
    .insufficient => .insufficient
  }

  inlineLtt:all e ~~ simplify[e],reform[e] |:
    (option[locn],cId,cExp,e,map[string,cDefn],integer) => e.
  inlineLtt(Lc,cId(Vr,Tp),Bnd,Exp,Prog,Depth) where cVar(_,VV).=Bnd =>
    simplify(Exp,Prog[Vr->vrDef(Lc,Vr,Tp,Bnd)],Depth).
  inlineLtt(Lc,cId(Vr,Tp),Bnd,Exp,Prog,Depth) where isGround(Bnd) =>
    simplify(Exp,Prog[Vr->vrDef(Lc,Vr,Tp,Bnd)],Depth).
  inlineLtt(Lc,Vr,Bnd,Exp,Prog,Depth) =>
    mkLtt(Lc,Vr,Bnd,simplify(Exp,Prog,Depth)).

  inlineCall:(option[locn],string,cons[cExp],tipe,map[string,cDefn],integer) => cExp.
  inlineCall(_,Nm,Args,_,Prog,Depth) where Depth>0 &&
      fnDef(Lc,_,_,Vrs,Rep) ^= Prog[Nm] =>
    simplifyExp(rewriteTerm(Rep,zip(Vrs//cName,Args)::map[string,cExp]),Prog[~Nm],Depth-1).
  inlineCall(Lc,Nm,Args,Tp,_,_) default => cCall(Lc,Nm,Args,Tp).

  inlineECall:(option[locn],string,cons[cExp],tipe,integer) => cExp.
  inlineECall(Lc,Nm,Args,Tp,Depth) where Depth>0 && {? A in Args *> isGround(A) ?} =>
    rewriteECall(Lc,Nm,Args,Tp).
  inlineECall(Lc,Nm,Args,Tp,_) default => cECall(Lc,Nm,Args,Tp).

  inlineOCall(Lc,cTerm(OLc,Nm,OArgs,_),Args,Tp,Prog,Depth) =>
    simplifyExp(cCall(Lc,Nm,OArgs++Args,Tp),Prog,Depth).
  inlineOCall(Lc,Op,Args,Tp,Prog,Depth) =>
    cOCall(Lc,Op,Args,Tp).
  
  rewriteECall(Lc,"_int_plus",[cInt(_,A),cInt(_,B)],_) => cInt(Lc,A+B).
  rewriteECall(Lc,"_int_minus",[cInt(_,A),cInt(_,B)],_) => cInt(Lc,A-B).
  rewriteECall(Lc,"_int_times",[cInt(_,A),cInt(_,B)],_) => cInt(Lc,A*B).
  rewriteECall(Lc,Op,Args,Tp) default => cECall(Lc,Op,Args,Tp).
  
}
