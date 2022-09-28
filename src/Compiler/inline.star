star.compiler.inline{
  import star.
  import star.sort.

  import star.compiler.data.
  import star.compiler.term.
  import star.compiler.freevars.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.

  public simplifyDefs:(cons[cDefn]) => cons[cDefn].
  simplifyDefs(Dfs) where Prog .= foldLeft(pickupDefn,[],Dfs) => (Dfs//(D)=>simplifyDefn(D,Prog)).

  pickupDefn:(cDefn,map[termLbl,cDefn])=>map[termLbl,cDefn].
  pickupDefn(fnDef(Lc,Nm,Tp,Args,Val),Map) =>
    Map[tLbl(Nm,arity(Tp))->fnDef(Lc,Nm,Tp,Args,Val)].
  pickupDefn(vrDef(Lc,Nm,Tp,Val),Map) => Map[tLbl(Nm,arity(Tp))->vrDef(Lc,Nm,Tp,Val)].
  pickupDefn(tpDef(_,_,_,_),Map) => Map.
  pickupDefn(lblDef(_,_,_,_),Map) => Map.

  simplifyDefn:(cDefn,map[termLbl,cDefn])=>cDefn.
  simplifyDefn(fnDef(Lc,Nm,Tp,Args,Val),Map) => 
    fnDef(Lc,Nm,Tp,Args,simplifyExp(Val,Map,10)).
  simplifyDefn(vrDef(Lc,Nm,Tp,Val),Map) => 
    vrDef(Lc,Nm,Tp,simplifyExp(Val,Map,10)).
  simplifyDefn(D,_) default => D.

  -- There are three possibilities of a match ...
  match[e] ::= .noMatch | .insufficient | matching(e).

  implementation all e ~~ display[e] |: display[match[e]] => {
    disp(.noMatch) => "noMatch".
    disp(.insufficient) => "insufficient".
    disp(matching(X)) => "matching $(X)"
  }

  -- ptnMatch tries to match an actual value with a pattern
  ptnMatch:(cExp,cExp,map[termLbl,cDefn]) => match[map[termLbl,cDefn]].
  ptnMatch(cVar(Lc1,cId(V1,T1)),E,Map) =>
    matching(Map[tLbl(V1,arity(T1))->vrDef(Lc1,V1,T1,E)]).
  ptnMatch(cInt(_,Ix),cInt(_,Ix),Map) => matching(Map).
  ptnMatch(cBig(_,Bx),cBig(_,Bx),Map) => matching(Map).
  ptnMatch(cFloat(_,Dx),cFloat(_,Dx),Map) => matching(Map).
  ptnMatch(cChar(_,Cx),cChar(_,Cx),Map) => matching(Map).
  ptnMatch(cString(_,Sx),cString(_,Sx),Map) => matching(Map).
  ptnMatch(cTerm(_,N,A1,_),cTerm(_,N,A2,_),Map) => ptnMatchArgs(A1,A2,Map).
  ptnMatch(cVoid(_,_),_,_) => .insufficient.  -- void on left does not match anything
  ptnMatch(_,cVoid(_,_),_) => .insufficient.  -- void on right does not match anything
  ptnMatch(_,_,_) default => .noMatch.

  ptnMatchArgs([],[],Map) => matching(Map).
  ptnMatchArgs([E1,..L1],[E2,..L2],Map) => case ptnMatch(E1,E2,Map) in {
    .noMatch => .noMatch.
    .insufficient => .insufficient.
    .matching(Ev) => ptnMatchArgs(L1,L2,Ev)
  }
  ptnMatchArgs(_,_,_) default => .noMatch.

  contract all e ~~ simplify[e] ::= {
    simplify:(e,map[termLbl,cDefn],integer) => e.
  }

  implementation simplify[cExp] => {
    simplify(E,Map,Dp) => simplifyExp(E,Map,Dp)
  }

  simplifyExp:(cExp,map[termLbl,cDefn],integer) => cExp.
  simplifyExp(E,P,D) => (D>0 ? simExp(E,P,D-1) || E).
  
  simExp(cVar(Lc,V),Map,Depth) => inlineVar(Lc,V,Map,Depth).
  simExp(cInt(Lc,Ix),_,_) => cInt(Lc,Ix).
  simExp(cBig(Lc,Bx),_,_) => cBig(Lc,Bx).
  simExp(cChar(Lc,Ix),_,_) => cChar(Lc,Ix).
  simExp(cFloat(Lc,Dx),_,_) => cFloat(Lc,Dx).
  simExp(cString(Lc,Sx),_,_) => cString(Lc,Sx).
  simExp(cVoid(Lc,Tp),_,_) => cVoid(Lc,Tp).
  simExp(cTerm(Lc,Fn,Args,Tp),Map,Depth) =>
    cTerm(Lc,Fn,Args//(A)=>simplifyExp(A,Map,Depth),Tp).
  simExp(cCall(Lc,Fn,Args,Tp),Map,Depth) =>
    inlineCall(Lc,Fn,Args//(A)=>simplifyExp(A,Map,Depth),Tp,Map,Depth).
  simExp(cECall(Lc,Fn,Args,Tp),Map,Depth) =>
    inlineECall(Lc,Fn,Args//(A)=>simplifyExp(A,Map,Depth),Tp,Depth).
  simExp(cOCall(Lc,Op,Args,Tp),Map,Depth) =>
    inlineOCall(Lc,simplifyExp(Op,Map,Depth),
      Args//(A)=>simplifyExp(A,Map,Depth),Tp,Map,Depth).
  simExp(cNth(Lc,T,Ix,Tp),Map,Depth) =>
    inlineTplOff(Lc,simplifyExp(T,Map,Depth),Ix,Tp).
  simExp(cSetNth(Lc,T,Ix,Vl),Map,Depth) =>
    applyTplUpdate(Lc,simplifyExp(T,Map,Depth),Ix,simplifyExp(Vl,Map,Depth)).
  simExp(cSeq(Lc,L,R),Map,Depth) =>
    cSeq(Lc,simplifyExp(L,Map,Depth),simplifyExp(R,Map,Depth)).
  simExp(cCnj(Lc,L,R),Map,Depth) =>
    applyCnj(Lc,simplifyExp(L,Map,Depth),simplifyExp(R,Map,Depth)).
  simExp(cDsj(Lc,L,R),Map,Depth) =>
    applyDsj(Lc,simplifyExp(L,Map,Depth),simplifyExp(R,Map,Depth)).
  simExp(cNeg(Lc,R),Map,Depth) =>
    applyNeg(Lc,simplifyExp(R,Map,Depth)).
  simExp(cCnd(Lc,T,L,R),Map,Depth) =>
    applyCnd(Lc,simplifyExp(T,Map,Depth),
      simplifyExp(L,Map,Depth),simplifyExp(R,Map,Depth)).
  simExp(cLtt(Lc,Vr,Bnd,Exp),Map,Depth) =>
    inlineLtt(Lc,Vr,simplifyExp(Bnd,Map,Depth),Exp,Map,Depth).
  simExp(cUnpack(Lc,Gov,Cases,Tp),Map,Depth) =>
    inlineUnpack(Lc,simplifyExp(Gov,Map,Depth),Cases,Map,Depth).
  simExp(cCase(Lc,Gov,Cases,Deflt,Tp),Map,Depth) =>
    inlineCase(Lc,simplifyExp(Gov,Map,Depth),Cases,
      simplifyExp(Deflt,Map,Depth),Map,Depth).
  simExp(cWhere(Lc,Ptn,Exp),Map,Depth) =>
    applyWhere(Lc,Ptn,simplifyExp(Exp,Map,Depth)).
  simExp(cMatch(Lc,Ptn,Exp),Map,Depth) =>
    applyMatch(Lc,Ptn,simplifyExp(Exp,Map,Depth)).
  simExp(cVarNmes(Lc,Vrs,Exp),Map,Depth) =>
    cVarNmes(Lc,Vrs,simplifyExp(Exp,Map,Depth)).
  simExp(cAbort(Lc,Txt,Tp),_,_) => cAbort(Lc,Txt,Tp).
  simExp(cSusp(Lc,Tsk,Evt,Tp),Map,Depth) =>
    cSusp(Lc,simplifyExp(Tsk,Map,Depth),simplifyExp(Evt,Map,Depth),Tp).
  simExp(cResume(Lc,Tsk,Evt,Tp),Map,Depth) =>
    cResume(Lc,simplifyExp(Tsk,Map,Depth),simplifyExp(Evt,Map,Depth),Tp).
  simExp(cTry(Lc,Exp,H,Tp),Map,Depth) =>
    cTry(Lc,simplifyExp(Exp,Map,Depth),simplifyExp(H,Map,Depth),Tp).
  simExp(cValof(Lc,Act,Tp),Map,Depth) =>
    cValof(Lc,simplifyAct(Act,Map,Depth),Tp).

  implementation simplify[aAction] => {
    simplify(A,Map,Dp) => simplifyAct(A,Map,Dp)
  }

  simplifyAct:(aAction,map[termLbl,cDefn],integer) => aAction.
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
  simAct(aCase(Lc,Gov,Cases,Deflt),Map,Depth) =>
    inlineCase(Lc,simplifyExp(Gov,Map,Depth),Cases,
      simplifyAct(Deflt,Map,Depth),Map,Depth).
  simAct(aUnpack(Lc,Gov,Cases),Map,Depth) =>
    inlineUnpack(Lc,simplifyExp(Gov,Map,Depth),Cases,Map,Depth).
  simAct(aIftte(Lc,T,L,R),Map,Depth) =>
    applyCnd(Lc,simplifyExp(T,Map,Depth),
      simplifyAct(L,Map,Depth),simplifyAct(R,Map,Depth)).
  simAct(aWhile(Lc,T,A),Map,Depth) =>
    aWhile(Lc,simplifyExp(T,Map,Depth),
      simplifyAct(A,Map,Depth)).
  simAct(aRetire(Lc,T,E),Map,Depth) =>
    aRetire(Lc,simplifyExp(T,Map,Depth),
      simplifyExp(E,Map,Depth)).
  simAct(aTry(Lc,T,H),Map,Depth) =>
    aTry(Lc,simplifyAct(T,Map,Depth),simplifyAct(H,Map,Depth)).
  simAct(aLtt(Lc,Vr,Bnd,A),Map,Depth) =>
    inlineLtt(Lc,Vr,simplifyExp(Bnd,Map,Depth),A,Map,Depth).
  simAct(aVarNmes(Lc,Vrs,X),Map,Depth) =>
    aVarNmes(Lc,Vrs,simplifyAct(X,Map,Depth)).
  simAct(aAbort(Lc,Txt),_,_) => aAbort(Lc,Txt).

  dropNops(_,aNop(_),A) => A.
  dropNops(_,A,aNop(_)) => A.
  dropNops(Lc,L,R) => aSeq(Lc,L,R).
  
  inlineVar(Lc,cId(Id,Tp),Map,Depth) where
      vrDef(_,_,_,Vl) ^= Map[tLbl(Id,arity(Tp))]/* && isGround(Vl) */ => simplify(Vl,Map,Depth-1).
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
    (option[locn],cExp,cons[cCase[e]],e,map[termLbl,cDefn],integer) => e.
  inlineCase(Lc,Gov,Cases,Deflt,Map,Depth) where 
      matching(Exp) .= matchingCase(Gov,Cases,Map,Depth) => Exp.
  inlineCase(Lc,Gov,Cases,Deflt,Map,Depth) =>
    mkCase(Lc,Gov,Cases,Deflt).

  inlineUnpack:all e ~~ rewrite[e], reform[e], simplify[e] |:
    (option[locn],cExp,cons[cCase[e]],map[termLbl,cDefn],integer) => e.
  inlineUnpack(Lc,Gov,Cases,Map,Depth) where
      matching(Exp) .= matchingCase(Gov,Cases,Map,Depth) => Exp.
  inlineUnpack(Lc,Gov,Cases,_,_) => mkUnpack(Lc,Gov,Cases).

  matchingCase:all e ~~ rewrite[e], reform[e], simplify[e] |:
    (cExp,cons[cCase[e]],map[termLbl,cDefn],integer) => match[e].
  matchingCase(_,[],_,_) => .noMatch.
  matchingCase(Gov,[C,..Cs],Map,Depth) => case candidate(Gov,C) in {
    .insufficient => .insufficient.
    .noMatch => matchingCase(Gov,Cs,Map,Depth).
    matching(Rep) => matching(simplify(Rep,Map,Depth))
  }

  candidate:all e ~~ rewrite[e] |: (cExp,cCase[e]) => match[e].
  candidate(E,(_,Ptn,Rep)) => case ptnMatch(Ptn,E,[]) in {
    matching(Theta) => matching(rewrite(Rep,Theta)).
    .noMatch => .noMatch.
    .insufficient => .insufficient
  }

  inlineLtt:all e ~~ simplify[e],reform[e],present[e],rewrite[e] |:
    (option[locn],cId,cExp,e,map[termLbl,cDefn],integer) => e.
  inlineLtt(Lc,cId(Vr,Tp),Bnd,Exp,Map,Depth) where isGround(Bnd) => valof{
    LttM = {tLbl(Vr,arity(Tp))->vrDef(Lc,Vr,Tp,Bnd)};
    valis simplify(rewrite(Exp,LttM),Map,Depth)
  }
  inlineLtt(Lc,Vr,Bnd,Exp,Map,Depth) =>
    mkLtt(Lc,Vr,Bnd,simplify(Exp,Map,Depth)).

  inlineCall:(option[locn],string,cons[cExp],tipe,map[termLbl,cDefn],integer) => cExp.
  inlineCall(Lc,Nm,Args,_Tp,Map,Depth) where Depth>0 &&
      PrgLbl .= tLbl(Nm,[|Args|]) &&
      fnDef(_,_,_,Vrs,Rep) ^= Map[PrgLbl] => valof{
	RwMap = { tLbl(VNm,arity(VTp))->vrDef(Lc,Nm,VTp,A) | (cId(VNm,VTp),A) in zip(Vrs,Args)};
	RwTerm = rewriteTerm(Rep,RwMap);
	valis (simplifyExp(RwTerm,Map[~PrgLbl],Depth-1))
      }
  inlineCall(Lc,Nm,Args,Tp,_,_) default => cCall(Lc,Nm,Args,Tp).

  inlineECall:(option[locn],string,cons[cExp],tipe,integer) => cExp.
  inlineECall(Lc,Nm,Args,Tp,Depth) where Depth>0 && {? A in Args *> isGround(A) ?} =>
    rewriteECall(Lc,Nm,Args,Tp).
  inlineECall(Lc,Nm,Args,Tp,_) default => cECall(Lc,Nm,Args,Tp).

  inlineOCall(Lc,cTerm(OLc,Nm,OArgs,_),Args,Tp,Map,Depth) =>
    simplifyExp(cCall(Lc,Nm,OArgs++Args,Tp),Map,Depth).
  inlineOCall(Lc,cVar(_,cId(Nm,VTp)),Args,Tp,Map,Depth) where Depth>0 &&
      Prg .= tLbl(Nm,arity(VTp)) &&
      vrDef(VLc,Nm,_,Rep) ^= Map[Prg] =>
    inlineOCall(Lc,simplifyExp(Rep,Map[~Prg],Depth-1),Args,Tp,Map,Depth).
  inlineOCall(Lc,Op,Args,Tp,Map,Depth) =>
    cOCall(Lc,Op,Args,Tp).
  
  rewriteECall(Lc,"_int_plus",[cInt(_,A),cInt(_,B)],_) => cInt(Lc,A+B).
  rewriteECall(Lc,"_int_minus",[cInt(_,A),cInt(_,B)],_) => cInt(Lc,A-B).
  rewriteECall(Lc,"_int_times",[cInt(_,A),cInt(_,B)],_) => cInt(Lc,A*B).
  rewriteECall(Lc,"_int_eq",[cInt(_,A),cInt(_,B)],_) =>
    cTerm(Lc,(A == B?"star.core#true"||"star.core#false"),[],boolType).
  rewriteECall(Lc,Op,Args,Tp) default => cECall(Lc,Op,Args,Tp).

  countOccs(cVoid(_,_),_Id,Cnt) => Cnt.
  countOccs(cAbort(_,_,_),_Id,Cnt) => Cnt.
  countOccs(cAnon(_,_),_Id,Cnt) => Cnt.
  countOccs(cVar(_,Vr),V,Cnt) => (V==Vr?Cnt+1||Cnt).
  countOccs(cInt(_,_),_Id,Cnt) => Cnt.
  countOccs(cChar(_,_),_Id,Cnt) => Cnt.
  countOccs(cBig(_,_),_Id,Cnt) => Cnt.
  countOccs(cFloat(_,_),_Id,Cnt) => Cnt.
  countOccs(cString(_,_),_Id,Cnt) => Cnt.
  countOccs(cTerm(_,_,Els,_),Id,Cnt) => countEls(Els,Id,Cnt).
  countOccs(cNth(_,E,_,_),Id,Cnt) => countOccs(E,Id,Cnt).
  countOccs(cSetNth(_,E,_,V),Id,Cnt) => countOccs(V,Id,countOccs(E,Id,Cnt)).
  countOccs(cCall(_,_,Els,_),Id,Cnt) => countEls(Els,Id,Cnt).
  countOccs(cECall(_,_,Els,_),Id,Cnt) => countEls(Els,Id,Cnt).
  countOccs(cOCall(_,Op,Els,_),Id,Cnt) => countEls(Els,Id,countOccs(Op,Id,Cnt)).
  countOccs(cThrow(_,E,_),Id,Cnt) => countOccs(E,Id,Cnt).
  countOccs(cSeq(_,L,R),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countOccs(cCnj(_,L,R),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countOccs(cDsj(_,L,R),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countOccs(cNeg(_,R),Id,Cnt) => countOccs(R,Id,Cnt).
  countOccs(cCnd(_,T,L,R),Id,Cnt) => countOccs(T,Id,countOccs(R,Id,countOccs(L,Id,Cnt))).
  countOccs(cLtt(_,V,L,R),Id,Cnt) => (V==Id ? Cnt || countOccs(L,Id,countOccs(R,Id,Cnt))).
  countOccs(cUnpack(_,G,Cs,_),Id,Cnt) => countCases(Cs,Id,countOccs(G,Id,Cnt),countOccs).
  countOccs(cCase(_,G,Cs,D,_),Id,Cnt) =>
    countCases(Cs,Id,countOccs(G,Id,countOccs(D,Id,Cnt)),countOccs).
  countOccs(cWhere(_,L,R),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countOccs(cMatch(_,L,R),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countOccs(cVarNmes(_,_,R),Id,Cnt) => countOccs(R,Id,Cnt).
  countOccs(cSusp(_,L,R,_),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countOccs(cResume(_,L,R,_),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countOccs(cTry(_,L,R,_),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countOccs(cValof(_,A,_),Id,Cnt) => countAOccs(A,Id,Cnt).

  countAOccs(aNop(_),_,Cnt) => Cnt.
  countAOccs(aAbort(_,_),_,Cnt) => Cnt.
  countAOccs(aSeq(_,L,R),Id,Cnt) => countAOccs(R,Id,countAOccs(L,Id,Cnt)).
  countAOccs(aLbld(_,_,A),Id,Cnt) => countAOccs(A,Id,Cnt).
  countAOccs(aBreak(_,_),_,Cnt) => Cnt.
  countAOccs(aValis(_,E),Id,Cnt) => countOccs(E,Id,Cnt).
  countAOccs(aThrow(_,E),Id,Cnt) => countOccs(E,Id,Cnt).
  countAOccs(aPerf(_,E),Id,Cnt) => countOccs(E,Id,Cnt).
  countAOccs(aDefn(_,L,_),Id,Cnt) where countOccs(L,Id,0)>0 => Cnt.
  countAOccs(aDefn(_,_,R),Id,Cnt) => countOccs(R,Id,Cnt).
  countAOccs(aAsgn(_,L,R),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countAOccs(aCase(_,L,Cs,D),Id,Cnt) =>
    countCases(Cs,Id,countAOccs(D,Id,countOccs(L,Id,Cnt)),countAOccs).
  countAOccs(aUnpack(_,L,Cs),Id,Cnt) =>
    countCases(Cs,Id,countOccs(L,Id,Cnt),countAOccs).
  countAOccs(aIftte(_,T,L,R),Id,Cnt) =>
    countAOccs(R,Id,countAOccs(L,Id,countOccs(T,Id,Cnt))).
  countAOccs(aWhile(_,T,L),Id,Cnt) =>
    countAOccs(L,Id,countOccs(T,Id,Cnt)).
  countAOccs(aRetire(_,L,R),Id,Cnt) => countOccs(R,Id,countOccs(L,Id,Cnt)).
  countAOccs(aTry(_,L,R),Id,Cnt) => countAOccs(R,Id,countAOccs(L,Id,Cnt)).
  countAOccs(aLtt(_,V,L,R),Id,Cnt) => (V==Id ? Cnt || countOccs(L,Id,countAOccs(R,Id,Cnt))).
  countAOccs(aVarNmes(_,_,R),Id,Cnt) => countAOccs(R,Id,Cnt).

  countEls([],_,Cnt) => Cnt.
  countEls([E,..Es],Id,Cnt) => countEls(Es,Id,countOccs(E,Id,Cnt)).

  countCases:all e ~~ (cons[cCase[e]],cId,integer,(e,cId,integer)=>integer)=>integer.
  countCases([],_,Cnt,_) => Cnt.
  countCases([(_,P,A),..As],Id,Cnt,F) => countCases(As,Id,countOccs(P,Id,F(A,Id,Cnt)),F).
}
