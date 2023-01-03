star.compiler.inline{
  import star.
  import star.sort.

  import star.compiler.data.
  import star.compiler.term.
  import star.compiler.freevars.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.

  public simplifyDefs:(cons[cDefn],map[termLbl,cDefn]) => cons[cDefn].
  simplifyDefs(Dfs,Prior) where Prog .= foldLeft(pickupDefn,Prior,Dfs) =>
    (Dfs//(D)=>simplifyDefn(D,Prog)).

  pickupDefn:(cDefn,map[termLbl,cDefn])=>map[termLbl,cDefn].
  pickupDefn(.fnDef(Lc,Nm,Tp,Args,Val),Map) =>
    Map[.tLbl(Nm,arity(Tp))->.fnDef(Lc,Nm,Tp,Args,Val)].
  pickupDefn(.vrDef(Lc,Nm,Tp,Val),Map) => Map[.tLbl(Nm,arity(Tp))->.vrDef(Lc,Nm,Tp,Val)].
  pickupDefn(.tpDef(_,_,_,_),Map) => Map.
  pickupDefn(.lblDef(_,_,_,_),Map) => Map.

  simplifyDefn:(cDefn,map[termLbl,cDefn])=>cDefn.
  simplifyDefn(.fnDef(Lc,Nm,Tp,Args,Val),Map) => 
    .fnDef(Lc,Nm,Tp,Args,simplifyExp(Val,Map[~.tLbl(Nm,[|Args|])],10)).
  simplifyDefn(.vrDef(Lc,Nm,Tp,Val),Map) => 
    .vrDef(Lc,Nm,Tp,simplifyExp(Val,Map,10)).
  simplifyDefn(D,_) default => D.

  -- There are three possibilities of a match ...
  match[e] ::= .noMatch | .insufficient | .matching(e).

  implementation all e ~~ display[e] |: display[match[e]] => {
    disp(.noMatch) => "noMatch".
    disp(.insufficient) => "insufficient".
    disp(.matching(X)) => "matching $(X)"
  }

  -- ptnMatch tries to match an actual value with a pattern
  ptnMatch:(cExp,cExp,map[termLbl,cDefn]) => match[map[termLbl,cDefn]].
  ptnMatch(.cVar(Lc1,.cId(V1,T1)),E,Map) =>
    .matching(Map[.tLbl(V1,arity(T1))->.vrDef(Lc1,V1,T1,E)]).
  ptnMatch(.cInt(_,Ix),.cInt(_,Ix),Map) => .matching(Map).
  ptnMatch(.cBig(_,Bx),.cBig(_,Bx),Map) => .matching(Map).
  ptnMatch(.cFloat(_,Dx),.cFloat(_,Dx),Map) => .matching(Map).
  ptnMatch(.cChar(_,Cx),.cChar(_,Cx),Map) => .matching(Map).
  ptnMatch(.cString(_,Sx),.cString(_,Sx),Map) => .matching(Map).
  ptnMatch(.cTerm(_,N,A1,_),.cTerm(_,N,A2,_),Map) => ptnMatchArgs(A1,A2,Map).
  ptnMatch(.cVoid(_,_),_,_) => .insufficient.  -- void on left does not match anything
  ptnMatch(_,.cVoid(_,_),_) => .insufficient.  -- void on right does not match anything
  ptnMatch(_,_,_) default => .noMatch.

  ptnMatchArgs([],[],Map) => .matching(Map).
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
  simplifyExp(E,P,D) where D>=0 => simExp(E,P,D).
  simplifyExp(E,_,_) => E.
  
  simExp(.cAnon(Lc,Tp),_Map,_Depth) => .cAnon(Lc,Tp).
  simExp(.cVar(Lc,V),Map,Depth) => inlineVar(Lc,V,Map,Depth).
  simExp(.cInt(Lc,Ix),_,_) => .cInt(Lc,Ix).
  simExp(.cBig(Lc,Bx),_,_) => .cBig(Lc,Bx).
  simExp(.cChar(Lc,Ix),_,_) => .cChar(Lc,Ix).
  simExp(.cFloat(Lc,Dx),_,_) => .cFloat(Lc,Dx).
  simExp(.cString(Lc,Sx),_,_) => .cString(Lc,Sx).
  simExp(.cVoid(Lc,Tp),_,_) => .cVoid(Lc,Tp).
  simExp(.cTerm(Lc,Fn,Args,Tp),Map,Depth) =>
    .cTerm(Lc,Fn,Args//(A)=>simExp(A,Map,Depth),Tp).
  simExp(.cCall(Lc,Fn,Args,Tp),Map,Depth) =>
    inlineCall(Lc,Fn,Args,Tp,Map,Depth).
  simExp(.cECall(Lc,Fn,Args,Tp),Map,Depth) =>
    inlineECall(Lc,Fn,Args//(A)=>simplifyExp(A,Map,Depth),Tp,Depth).
  simExp(.cOCall(Lc,Op,Args,Tp),Map,Depth) => 
    inlineOCall(Lc,simExp(Op,Map,Depth),Args//(A)=>simExp(A,Map,Depth),Tp,Map,Depth).
  simExp(.cNth(Lc,T,Ix,Tp),Map,Depth) =>
    inlineTplOff(Lc,simExp(T,Map,Depth),Ix,Tp).
  simExp(.cSetNth(Lc,T,Ix,Vl),Map,Depth) =>
    applyTplUpdate(Lc,simExp(T,Map,Depth),Ix,simExp(Vl,Map,Depth)).
  simExp(.cThunk(Lc,T,Tp),Map,Depth) =>
    .cThunk(Lc,simExp(T,Map,Depth),Tp).
  simExp(.cSeq(Lc,L,R),Map,Depth) =>
    .cSeq(Lc,simExp(L,Map,Depth),simExp(R,Map,Depth)).
  simExp(.cCnj(Lc,L,R),Map,Depth) =>
    applyCnj(Lc,simExp(L,Map,Depth),simExp(R,Map,Depth)).
  simExp(.cDsj(Lc,L,R),Map,Depth) =>
    applyDsj(Lc,simExp(L,Map,Depth),simExp(R,Map,Depth)).
  simExp(.cNeg(Lc,R),Map,Depth) =>
    applyNeg(Lc,simExp(R,Map,Depth)).
  simExp(.cCnd(Lc,T,L,R),Map,Depth) =>
    applyCnd(Lc,simExp(T,Map,Depth),
      simExp(L,Map,Depth),simExp(R,Map,Depth)).
  simExp(.cLtt(Lc,Vr,Bnd,Exp),Map,Depth) =>
    inlineLtt(Lc,Vr,simplifyExp(Bnd,Map,Depth),Exp,Map,Depth).
  simExp(.cCont(Lc,Vr,Bnd,Exp),Map,Depth) =>
    inlineCont(Lc,Vr,simplifyExp(Bnd,Map,Depth),Exp,Map,Depth).
  simExp(.cUnpack(Lc,Gov,Cases,Tp),Map,Depth) =>
    inlineUnpack(Lc,simplifyExp(Gov,Map,Depth),Cases,Map,Depth).
  simExp(.cCase(Lc,Gov,Cases,Deflt,Tp),Map,Depth) =>
    inlineCase(Lc,simplifyExp(Gov,Map,Depth),Cases,
      simplifyExp(Deflt,Map,Depth),Map,Depth).
  simExp(.cMatch(Lc,Ptn,Exp),Map,Depth) =>
    applyMatch(Lc,simplifyExp(Ptn,Map,Depth),simplifyExp(Exp,Map,Depth)).
  simExp(.cVarNmes(Lc,Vrs,Exp),Map,Depth) =>
    .cVarNmes(Lc,Vrs,simplifyExp(Exp,Map,Depth)).
  simExp(.cAbort(Lc,Txt,Tp),_,_) => .cAbort(Lc,Txt,Tp).
  simExp(.cSusp(Lc,Tsk,Evt,Tp),Map,Depth) =>
    .cSusp(Lc,simplifyExp(Tsk,Map,Depth),simplifyExp(Evt,Map,Depth),Tp).
  simExp(.cResume(Lc,Tsk,Evt,Tp),Map,Depth) =>
    .cResume(Lc,simplifyExp(Tsk,Map,Depth),simplifyExp(Evt,Map,Depth),Tp).
  simExp(.cThrow(Lc,Th,E,Tp),Map,Depth) =>
    .cThrow(Lc,simplifyExp(Th,Map,Depth),simplifyExp(E,Map,Depth),Tp).
  simExp(.cTry(Lc,Exp,Th,E,H,Tp),Map,Depth) =>
    .cTry(Lc,simplifyExp(Exp,Map,Depth),simplifyExp(Th,Map,Depth),
      simplifyExp(E,Map,Depth),simplifyExp(H,Map,Depth),Tp).
  simExp(.cValof(Lc,Act,Tp),Map,Depth) =>
    valofAct(Lc,simplifyAct(Act,Map,Depth),Tp).

  valofAct(_,.aValis(_Lc,E),_) => E.
  valofAct(Lc,A,Tp) => .cValof(Lc,A,Tp).
  
  implementation simplify[aAction] => {
    simplify(A,Map,Dp) => simplifyAct(A,Map,Dp)
  }

  simplifyAct:(aAction,map[termLbl,cDefn],integer) => aAction.
  simplifyAct(A,P,D) => simAct(A,P,D).

  simAct(.aNop(Lc),_,_) => .aNop(Lc).
  simAct(.aSeq(Lc,A1,A2),Map,Depth) =>
    dropNops(Lc,simplifyAct(A1,Map,Depth),simplifyAct(A2,Map,Depth)).
  simAct(.aLbld(Lc,Lb,A),Map,Depth) => valof{
    AA = simplifyAct(A,Map,Depth);
    if lblUsed(AA,Lb) then
      valis .aLbld(Lc,Lb,AA)
    else
    valis AA
  }
  simAct(.aBreak(Lc,Lb),_,_) => .aBreak(Lc,Lb).
  simAct(.aValis(Lc,E),Map,Depth) => getValis(Lc,simplifyExp(E,Map,Depth)).
  simAct(.aThrow(Lc,T,E),Map,Depth) => .aThrow(Lc,simplifyExp(T,Map,Depth),simplifyExp(E,Map,Depth)).
  simAct(.aPerf(Lc,E),Map,Depth) => .aPerf(Lc,simplifyExp(E,Map,Depth)).
  simAct(.aDefn(Lc,V,E),Map,Depth) =>
    .aDefn(Lc,simplifyExp(V,Map,Depth),simplifyExp(E,Map,Depth)).
  simAct(.aAsgn(Lc,V,E),Map,Depth) =>
    .aAsgn(Lc,simplifyExp(V,Map,Depth),simplifyExp(E,Map,Depth)).
  simAct(.aSetNth(Lc,T,Ix,E),Map,Depth) =>
    .aSetNth(Lc,simplifyExp(T,Map,Depth),Ix,simplifyExp(E,Map,Depth)).
  simAct(.aCase(Lc,Gov,Cases,Deflt),Map,Depth) =>
    inlineCase(Lc,simplifyExp(Gov,Map,Depth),Cases,
      simplifyAct(Deflt,Map,Depth),Map,Depth).
  simAct(.aUnpack(Lc,Gov,Cases),Map,Depth) =>
    inlineUnpack(Lc,simplifyExp(Gov,Map,Depth),Cases,Map,Depth).
  simAct(.aIftte(Lc,T,L,R),Map,Depth) =>
    applyCnd(Lc,simplifyExp(T,Map,Depth),
      simplifyAct(L,Map,Depth),simplifyAct(R,Map,Depth)).
  simAct(.aWhile(Lc,T,A),Map,Depth) =>
    .aWhile(Lc,simplifyExp(T,Map,Depth),
      simplifyAct(A,Map,Depth)).
  simAct(.aRetire(Lc,T,E),Map,Depth) =>
    .aRetire(Lc,simplifyExp(T,Map,Depth),
      simplifyExp(E,Map,Depth)).
  simAct(.aTry(Lc,B,T,E,H),Map,Depth) =>
    .aTry(Lc,simplifyAct(B,Map,Depth),
      simplifyExp(T,Map,Depth),simplifyExp(E,Map,Depth),simplifyAct(H,Map,Depth)).
  simAct(.aLtt(Lc,Vr,Bnd,A),Map,Depth) =>
    inlineLtt(Lc,Vr,simplifyExp(Bnd,Map,Depth),A,Map,Depth).
  simAct(.aCont(Lc,Vr,Bnd,A),Map,Depth) =>
    inlineCont(Lc,Vr,simplifyExp(Bnd,Map,Depth),A,Map,Depth).
  simAct(.aVarNmes(Lc,Vrs,X),Map,Depth) =>
    .aVarNmes(Lc,Vrs,simplifyAct(X,Map,Depth)).
  simAct(.aAbort(Lc,Txt),_,_) => .aAbort(Lc,Txt).

  dropNops(_,.aNop(_),A) => A.
  dropNops(_,A,.aNop(_)) => A.
  dropNops(Lc,L,R) => .aSeq(Lc,L,R).

  getValis(Lc,.cValof(_,A,_)) => A.
  getValis(Lc,E) => .aValis(Lc,E).
  
  inlineVar(Lc,.cId("_",Tp),_Map,_Depth) => .cAnon(Lc,Tp).
  inlineVar(Lc,.cId(Id,Tp),Map,Depth) where
      .vrDef(_,_,_,Vl) ?= Map[.tLbl(Id,arity(Tp))] && isGround(Vl) => simplify(Vl,Map,Depth).
  inlineVar(Lc,V,_,_) => .cVar(Lc,V).

  applyCnj(_,.cTerm(_,"star.core#true",[],_),R) => R.
  applyCnj(_,.cTerm(Lc,"star.core#false",[],Tp),R) => .cTerm(Lc,"star.core#false",[],Tp).
  applyCnj(Lc,L,R) => .cCnj(Lc,L,R).

  applyDsj(_,.cTerm(_,"star.core#false",[],_),R) => R.
  applyDsj(_,.cTerm(Lc,"star.core#true",[],Tp),R) => 
    .cTerm(Lc,"star.core#false",[],Tp).
  applyDsj(Lc,L,R) => .cDsj(Lc,L,R).

  applyNeg(_,.cTerm(Lc,"star.core#false",[],Tp)) => .cTerm(Lc,"star.core#true",[],Tp).
  applyNeg(_,.cTerm(Lc,"star.core#true",[],Tp)) => .cTerm(Lc,"star.core#false",[],Tp).
  applyNeg(Lc,Inner) => .cNeg(Lc,Inner).

  applyCnd:all e ~~ reform[e] |: (option[locn],cExp,e,e) => e.
  applyCnd(_,.cTerm(_,"star.core#false",[],_),_L,R) => R.
  applyCnd(_,.cTerm(_,"star.core#true",[],_),L,_R) => L.
  applyCnd(Lc,T,L,R) => mkCond(Lc,T,L,R).

  inlineTplOff(_,.cTerm(_,_,Els,_),Ix,Tp) where E?=Els[Ix] => E.
  inlineTplOff(Lc,T,Ix,Tp) default => .cNth(Lc,T,Ix,Tp).
  
  applyTplUpdate(_,.cTerm(Lc,Nm,Args,Tp),Ix,E) =>
    .cTerm(Lc,Nm,Args[Ix->E],Tp).
  applyTplUpdate(Lc,T,Ix,E) =>
    .cSetNth(Lc,T,Ix,E).

  applyMatch(Lc,Ptn,Exp) where isGround(Ptn) && isGround(Exp) =>
    (Ptn==Exp ?? .cTerm(Lc,"star.core#true",[],boolType) || .cTerm(Lc,"star.core#false",[],boolType)).
  applyMatch(Lc,Ptn,Exp) => .cMatch(Lc,Ptn,Exp).

  inlineCase:all e ~~ rewrite[e], reform[e], simplify[e] |:
    (option[locn],cExp,cons[cCase[e]],e,map[termLbl,cDefn],integer) => e.
  inlineCase(Lc,Gov,Cases,Deflt,Map,Depth) where 
      .matching(Exp) .= matchingCase(Gov,Cases,Map,Depth) => Exp.
  inlineCase(Lc,Gov,Cases,Deflt,Map,Depth) =>
    mkCase(Lc,Gov,Cases,Deflt).

  inlineUnpack:all e ~~ rewrite[e], reform[e], simplify[e] |:
    (option[locn],cExp,cons[cCase[e]],map[termLbl,cDefn],integer) => e.
  inlineUnpack(Lc,Gov,Cases,Map,Depth) where
      .matching(Exp) .= matchingCase(Gov,Cases,Map,Depth) => Exp.
  inlineUnpack(Lc,Gov,Cases,_,_) => mkUnpack(Lc,Gov,Cases).

  matchingCase:all e ~~ rewrite[e], reform[e], simplify[e] |:
    (cExp,cons[cCase[e]],map[termLbl,cDefn],integer) => match[e].
  matchingCase(_,[],_,_) => .noMatch.
  matchingCase(Gov,[C,..Cs],Map,Depth) => case candidate(Gov,C) in {
    .insufficient => .insufficient.
    .noMatch => matchingCase(Gov,Cs,Map,Depth).
    .matching(Rep) => .matching(simplify(Rep,Map,Depth))
  }.

  candidate:all e ~~ rewrite[e] |: (cExp,cCase[e]) => match[e].
  candidate(E,(_,Ptn,Rep)) => case ptnMatch(Ptn,E,[]) in {
    .matching(Theta) => .matching(rewrite(Rep,Theta)).
    .noMatch => .noMatch.
    .insufficient => .insufficient
  }

  inlineLtt:all e ~~ simplify[e],reform[e],present[e],rewrite[e] |:
    (option[locn],cId,cExp,e,map[termLbl,cDefn],integer) => e.
  inlineLtt(Lc,.cId(Vr,Tp),Bnd,Exp,Map,Depth) where isGround(Bnd) => valof{
    LttM = { .tLbl(Vr,arity(Tp))->.vrDef(Lc,Vr,Tp,Bnd)};
    valis simplify(rewrite(Exp,LttM),Map,Depth)
  }
  inlineLtt(Lc,Vr,Bnd,Exp,Map,Depth) =>
    mkLtt(Lc,Vr,Bnd,simplify(Exp,Map,Depth)).

  inlineCont:all e ~~ simplify[e],reform[e],present[e],rewrite[e] |:
    (option[locn],cId,cExp,e,map[termLbl,cDefn],integer) => e.
  inlineCont(Lc,Vr,Bnd,Exp,Map,Depth) where present(Exp,varFound(Vr)) =>
    mkCont(Lc,Vr,Bnd,simplify(Exp,Map,Depth)).
  inlineCont(Lc,Vr,Bnd,Exp,Map,Depth) => simplify(Exp,Map,Depth).

  varFound(Vr) => (T)=>(.cVar(_,VV).=T ?? VV==Vr || .false).
  
  inlineCall:(option[locn],string,cons[cExp],tipe,map[termLbl,cDefn],integer) => cExp.
  inlineCall(Lc,Nm,Args,_Tp,Map,Depth) where Depth>0 &&
      PrgLbl .= .tLbl(Nm,[|Args|]) && .fnDef(_,_,_,Vrs,Rep) ?= Map[PrgLbl] => valof{
	RwMap = { .tLbl(VNm,arity(VTp))->.vrDef(Lc,Nm,VTp,A) | (.cId(VNm,VTp),A) in zip(Vrs,Args)};
	valis simplifyExp(rewriteTerm(Rep,RwMap),Map[~PrgLbl],Depth)
      }.
  inlineCall(Lc,Nm,Args,Tp,Map,Depth) default => .cCall(Lc,Nm,Args//(A)=>simplifyExp(A,Map,Depth),Tp).

  inlineECall:(option[locn],string,cons[cExp],tipe,integer) => cExp.
  inlineECall(Lc,Nm,Args,Tp,Depth) where Depth>0 && {? A in Args *> isGround(A) ?} =>
    rewriteECall(Lc,Nm,Args,Tp).
  inlineECall(Lc,Nm,Args,Tp,_) default => .cECall(Lc,Nm,Args,Tp).

  inlineOCall(Lc,.cTerm(OLc,Nm,OArgs,_),Args,Tp,Map,Depth) =>
    simplifyExp(.cCall(Lc,Nm,OArgs++Args,Tp),Map,Depth).
  inlineOCall(Lc,Op,Args,Tp,Map,Depth) => .cOCall(Lc,Op,Args,Tp).
  
  rewriteECall(Lc,"_int_plus",[.cInt(_,A),.cInt(_,B)],_) => .cInt(Lc,A+B).
  rewriteECall(Lc,"_int_minus",[.cInt(_,A),.cInt(_,B)],_) => .cInt(Lc,A-B).
  rewriteECall(Lc,"_int_times",[.cInt(_,A),.cInt(_,B)],_) => .cInt(Lc,A*B).
  rewriteECall(Lc,"_int_eq",[.cInt(_,A),.cInt(_,B)],_) =>
    .cTerm(Lc,(A == B??"star.core#true"||"star.core#false"),[],boolType).
  rewriteECall(Lc,"_int_lt",[.cInt(_,A),.cInt(_,B)],_) =>
    .cTerm(Lc,(A < B??"star.core#true"||"star.core#false"),[],boolType).
  rewriteECall(Lc,"_int_ge",[.cInt(_,A),.cInt(_,B)],_) =>
    .cTerm(Lc,(A >= B??"star.core#true"||"star.core#false"),[],boolType).
  rewriteECall(Lc,"_str_multicatε",[As],_) where isGround(As) =>
    .cString(Lc,pullStrings(As)*).
  rewriteECall(Lc,Op,Args,Tp) default => .cECall(Lc,Op,Args,Tp).

  pullStrings(.cTerm(_,"star.core#nil",[],_)) => [].
  pullStrings(.cTerm(_,"star.core#cons",[.cString(_,S),Tl],_)) => [S,..pullStrings(Tl)].
  
  countOccs:(cExp,cId,integer) => integer.
  countOccs(T,Id,Cnt) => case T in {
    .cVoid(_,_) => Cnt.
    .cAbort(_,_,_) => Cnt.
    .cAnon(_,_) => Cnt.
    .cVar(_,Vr) => (Id==Vr??Cnt+1||Cnt).
    .cInt(_,_) => Cnt.
    .cChar(_,_) => Cnt.
    .cBig(_,_) => Cnt.
    .cFloat(_,_) => Cnt.
    .cString(_,_) => Cnt.
    .cTerm(_,_,Els,_) => countEls(Els,Id,Cnt).
    .cNth(_,E,_,_) => countOccs(E,Id,Cnt).
    .cSetNth(_,E,_,V) => countOccs(V,Id,countOccs(E,Id,Cnt)).
    .cThunk(_,E,_) => countOccs(E,Id,Cnt).
    .cCall(_,_,Els,_) => countEls(Els,Id,Cnt).
    .cECall(_,_,Els,_) => countEls(Els,Id,Cnt).
    .cOCall(_,Op,Els,_) => countEls(Els,Id,countOccs(Op,Id,Cnt)).
    .cThrow(_,T,E,_) => countOccs(T,Id,countOccs(E,Id,Cnt)).
    .cSeq(_,L,R) => countOccs(R,Id,countOccs(L,Id,Cnt)).
    .cCnj(_,L,R) => countOccs(R,Id,countOccs(L,Id,Cnt)).
    .cDsj(_,L,R) => countOccs(R,Id,countOccs(L,Id,Cnt)).
    .cNeg(_,R) => countOccs(R,Id,Cnt).
    .cCnd(_,T,L,R) => countOccs(T,Id,countOccs(R,Id,countOccs(L,Id,Cnt))).
    .cLtt(_,V,L,R) => (V==Id ?? Cnt || countOccs(L,Id,countOccs(R,Id,Cnt))).
    .cCont(_,V,L,R) => (V==Id ?? Cnt || countOccs(L,Id,countOccs(R,Id,Cnt))).
    .cUnpack(_,G,Cs,_) => countCases(Cs,Id,countOccs(G,Id,Cnt),countOccs).
    .cCase(_,G,Cs,D,_) =>
      countCases(Cs,Id,countOccs(G,Id,countOccs(D,Id,Cnt)),countOccs).
    .cMatch(_,L,R) => countOccs(R,Id,countOccs(L,Id,Cnt)).
    .cVarNmes(_,_,R) => countOccs(R,Id,Cnt).
    .cSusp(_,L,R,_) => countOccs(R,Id,countOccs(L,Id,Cnt)).
    .cResume(_,L,R,_) => countOccs(R,Id,countOccs(L,Id,Cnt)).
    .cTry(_,L,T,E,R,_) => countOccs(T,Id,countOccs(E,Id,countOccs(R,Id,countOccs(L,Id,Cnt)))).
    .cValof(_,A,_) => countAOccs(A,Id,Cnt).
  }

  countAOccs(A,Id,Cnt) => case A in {
    .aNop(_) => Cnt.
    .aAbort(_,_) => Cnt.
    .aSeq(_,L,R) => countAOccs(R,Id,countAOccs(L,Id,Cnt)).
    .aLbld(_,_,A) => countAOccs(A,Id,Cnt).
    .aBreak(_,_) => Cnt.
    .aValis(_,E) => countOccs(E,Id,Cnt).
    .aThrow(_,T,E) => countOccs(T,Id,countOccs(E,Id,Cnt)).
    .aPerf(_,E) => countOccs(E,Id,Cnt).
    .aDefn(_,L,_) where countOccs(L,Id,0)>0 => Cnt.
    .aDefn(_,_,R) => countOccs(R,Id,Cnt).
    .aAsgn(_,L,R) => countOccs(R,Id,countOccs(L,Id,Cnt)).
    .aCase(_,L,Cs,D) =>
      countCases(Cs,Id,countAOccs(D,Id,countOccs(L,Id,Cnt)),countAOccs).
    .aUnpack(_,L,Cs) =>
      countCases(Cs,Id,countOccs(L,Id,Cnt),countAOccs).
    .aIftte(_,T,L,R) =>
      countAOccs(R,Id,countAOccs(L,Id,countOccs(T,Id,Cnt))).
    .aWhile(_,T,L) => countAOccs(L,Id,countOccs(T,Id,Cnt)).
    .aRetire(_,L,R) => countOccs(R,Id,countOccs(L,Id,Cnt)).
    .aTry(_,L,T,E,R) => countOccs(E,Id,countOccs(T,Id,countAOccs(R,Id,countAOccs(L,Id,Cnt)))).
    .aLtt(_,V,L,R) => (V==Id ?? Cnt || countOccs(L,Id,countAOccs(R,Id,Cnt))).
    .aCont(_,V,L,R) => (V==Id ?? Cnt || countOccs(L,Id,countAOccs(R,Id,Cnt))).
    .aVarNmes(_,_,R) => countAOccs(R,Id,Cnt).
  }

  countEls([],_,Cnt) => Cnt.
  countEls([E,..Es],Id,Cnt) => countEls(Es,Id,countOccs(E,Id,Cnt)).

  countCases:all e ~~ (cons[cCase[e]],cId,integer,(e,cId,integer)=>integer)=>integer.
  countCases([],_,Cnt,_) => Cnt.
  countCases([(_,P,A),..As],Id,Cnt,F) => countCases(As,Id,countOccs(P,Id,F(A,Id,Cnt)),F).
}
