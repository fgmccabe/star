star.compiler.inline{
  import star.
  import star.sort.

  import star.compiler.data.
  import star.compiler.dependencies.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.term.
  import star.compiler.escapes.
  import star.compiler.freevars.
  import star.compiler.freshen.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.opts.
  import star.compiler.types.
  import star.compiler.unify.

  import star.compiler.location.

  -- Process definitions in a way that avoids recursion,
  -- by first of all topogical sorting

  public simplifyDefs:(cons[cDefn],cons[cDefn]) => cons[cDefn].
  simplifyDefs(Imported,Dfs) => simplifyGroups(sortDefs(Dfs),foldLeft(pickupDefn,[],Imported))*.

  simplifyGroups:(cons[cons[cDefn]],map[defnSp,cDefn]) => cons[cons[cDefn]].
  simplifyGroups([],_) => [].
  simplifyGroups([Gp,..Gps],Map) => valof{
    SGp = Gp//(D)=>simplifyDefn(D,Map);
    if traceInline! then
      showMsg("Group simplifies to $(SGp)");

    valis [SGp,..simplifyGroups(Gps,foldLeft(pickupDefn,Map,SGp))]
  }

  pickupDefn:(cDefn,map[defnSp,cDefn])=>map[defnSp,cDefn].
  pickupDefn(Df,Map) => case Df in {
    | .fnDef(_,Nm,_,_,_) => Map[.varSp(Nm)->Df]
    | .prDef(_,Nm,_,_,_) => Map[.varSp(Nm)->Df]
    | .glDef(_,Nm,_,_) => Map[.varSp(Nm)->Df]
    | .tpDef(_,Tp,_,_) => Map[.tpSp(tpName(Tp))->Df]
    | .lblDef(_,_,_,_) => Map
    }.

  simplifyDefn:(cDefn,map[defnSp,cDefn])=>cDefn.
  simplifyDefn(.fnDef(Lc,Nm,Tp,Args,FnBody),Map) =>
    .fnDef(Lc,Nm,Tp,Args,simplifyExp(FnBody,Map[~.varSp(Nm)],10)).
  simplifyDefn(.prDef(Lc,Nm,Tp,Args,Act),Map) =>
    .prDef(Lc,Nm,Tp,Args,simplifyAct(Act,Map[~.varSp(Nm)],10)).
  simplifyDefn(.glDef(Lc,Nm,Tp,GVal),Map) =>
    .glDef(Lc,Nm,Tp,simplifyExp(GVal,Map,4)).
  simplifyDefn(D,_) default => D.

  -- There are three possibilities of a match ...
  match[e] ::= .noMatch | .insufficient | .matching(e).

  implementation all e ~~ display[e] |= display[match[e]] => {
    disp(.noMatch) => "noMatch".
    disp(.insufficient) => "insufficient".
    disp(.matching(X)) => "matching $(X)"
  }

  -- ptnMatch tries to match an actual value with a pattern
  ptnMatch:(cExp,cExp,map[string,cExp]) => match[map[string,cExp]].
  ptnMatch(.cVar(Lc1,.cV(V1,T1)),E,Map) => .matching(Map[V1->E]).
  ptnMatch(.cInt(_,Ix),.cInt(_,Ix),Map) => .matching(Map).
  ptnMatch(.cBig(_,Bx),.cBig(_,Bx),Map) => .matching(Map).
  ptnMatch(.cFlt(_,Dx),.cFlt(_,Dx),Map) => .matching(Map).
  ptnMatch(.cChar(_,Cx),.cChar(_,Cx),Map) => .matching(Map).
  ptnMatch(.cString(_,Sx),.cString(_,Sx),Map) => .matching(Map).
  ptnMatch(.cTerm(_,N,A1,_),.cTerm(_,N,A2,_),Map) => ptnMatchArgs(A1,A2,Map).
  ptnMatch(.cVoid(_),_,_) => .insufficient.  -- void on left does not match anything
  ptnMatch(_,.cVoid(_),_) => .insufficient.  -- void on right does not match anything
  ptnMatch(_,_,_) default => .noMatch.

  ptnMatchArgs([],[],Map) => .matching(Map).
  ptnMatchArgs([E1,..L1],[E2,..L2],Map) => case ptnMatch(E1,E2,Map) in {
    | .noMatch => .noMatch
    | .insufficient => .insufficient
    | .matching(Ev) => ptnMatchArgs(L1,L2,Ev)
  }
  ptnMatchArgs(_,_,_) default => .noMatch.

  contract all e ~~ simplify[e] ::= {
    simplify:(e,map[defnSp,cDefn],integer) => e.
  }

  implementation simplify[cExp] => {
    simplify(E,Map,Dp) => simplifyExp(E,Map,Dp)
  }

  simplifyExp:(cExp,map[defnSp,cDefn],integer) => cExp.
  simplifyExp(E,P,D) where D>=0 => simExp(E,P,D).
  simplifyExp(E,_,_) => E.

  simExp(Exp,Map,Depth) => case Exp in {
    | .cVoid(_) => Exp
    | .cAnon(_,_) => Exp
    | .cUnrch(_,_) => Exp
    | .cVar(Lc,V) => inlineVar(Lc,V,Map,Depth)
    | .cInt(Lc,Ix) => .cInt(Lc,Ix)
    | .cBig(Lc,Bx) => .cBig(Lc,Bx)
    | .cChar(Lc,Ix) => .cChar(Lc,Ix)
    | .cFlt(Lc,Dx) => .cFlt(Lc,Dx)
    | .cString(Lc,Sx) => .cString(Lc,Sx)
    | .cTerm(Lc,Fn,Args,Tp) => .cTerm(Lc,Fn,Args//(A)=>simExp(A,Map,Depth),Tp)
    | .cCall(Lc,Nm,Args,Tp) where isEscape(Nm) =>
      inlineECall(Lc,Nm,Args//(A)=>simplifyExp(A,Map,Depth),Tp,Depth)
    | .cCall(Lc,Fn,Args,Tp) => inlineCall(Lc,Fn,Args,Tp,Map,Depth)
    | .cXCall(Lc,Fn,Args,Tp,ErTp) => inlineXCall(Lc,Fn,Args,Tp,ErTp,Map,Depth)
    | .cOCall(Lc,Op,Args,Tp) => inlineOCall(Lc,simExp(Op,Map,Depth),Args,Tp,Map,Depth)
    | .cXOCall(Lc,Op,Args,Tp,ErTp) => inlineXOCall(Lc,simExp(Op,Map,Depth),Args,Tp,ErTp,Map,Depth)
    | .cXOCall(Lc,Op,Args,Tp,ErTp) => .cXOCall(Lc,simExp(Op,Map,Depth),Args//(A)=>simExp(A,Map,Depth),Tp,ErTp)
    | .cNth(Lc,T,Ix,Tp) => inlineTplOff(Lc,simExp(T,Map,Depth),Ix,Tp)
    | .cSetNth(Lc,T,Ix,Vl) => applyTplUpdate(Lc,simExp(T,Map,Depth),Ix,simExp(Vl,Map,Depth))
    | .cClos(Lc,Lb,Ar,Fr,Tp) => .cClos(Lc,Lb,Ar,simExp(Fr,Map,Depth),Tp)
    | .cCel(Lc,E,Tp) => .cCel(Lc,simExp(E,Map,Depth),Tp)
    | .cGet(Lc,E,Tp) => .cGet(Lc,simExp(E,Map,Depth),Tp)
    | .cSv(_,_) => Exp
    | .cSvDrf(Lc,Th,Tp) => .cSvDrf(Lc,simExp(Th,Map,Depth),Tp)
    | .cSvSet(Lc,T,V) => .cSvSet(Lc,simExp(T,Map,Depth),simExp(V,Map,Depth))
    | .cSeq(Lc,L,R) => .cSeq(Lc,simExp(L,Map,Depth),simExp(R,Map,Depth))
    | .cCnj(_,_,_) => simCond(Exp,Map,Depth)
    | .cDsj(_,_,_) => simCond(Exp,Map,Depth.>>.1)
    | .cNeg(_,_) => simCond(Exp,Map,Depth)
    | .cCnd(_,_,_,_) => simCond(Exp,Map,Depth)
    | .cLtt(Lc,Vr,Bnd,Inn) => valof{
      NE = genVar(vName(Vr),typeOf(Vr));
      valis inlineLtt(Lc,NE,simplifyExp(Bnd,Map,Depth),
	freshenE(Inn,{lName(Vr)->.cVar(Lc,NE)}),Map,Depth)
    }
    | .cCase(Lc,Gov,Cases,Deflt,Tp) =>
      inlineCase(Lc,simplifyExp(Gov,Map,Depth),Cases,simplifyExp(Deflt,Map,Depth),Map,Depth)
    | .cIxCase(Lc,Gov,Cases,Deflt,Tp) =>
      inlineIndex(Lc,simplifyExp(Gov,Map,Depth),Cases,simplifyExp(Deflt,Map,Depth),Map,Depth)
    | .cMatch(Lc,Ptn,Val) =>
      applyMatch(Lc,simplifyExp(Ptn,Map,Depth),simplifyExp(Val,Map,Depth),Map,Depth)
    | .cAbort(Lc,Txt,Tp) => .cAbort(Lc,Txt,Tp)
    | .cThrw(Lc,E,Tp) => .cThrw(Lc,simplifyExp(E,Map,Depth),Tp)
    | .cTry(Lc,Inn,E,H,Tp) where .cVar(VLc,EV) .= E => valof{
      NE = genVar(vName(EV),typeOf(EV));
      NH = simplifyExp(freshenE(H,{lName(EV)->.cVar(VLc,NE)}),Map,Depth);
      valis .cTry(Lc,simplifyExp(Inn,Map,Depth),.cVar(VLc,NE),NH,Tp)
    }
    | .cSusp(Lc,T,M,Tp) =>
      .cSusp(Lc,simplifyExp(T,Map,Depth),simplifyExp(M,Map,Depth),Tp)
    | .cRetyr(Lc,T,M,Tp) =>
      .cRetyr(Lc,simplifyExp(T,Map,Depth),simplifyExp(M,Map,Depth),Tp)
    | .cResum(Lc,T,M,Tp) =>
      .cResum(Lc,simplifyExp(T,Map,Depth),simplifyExp(M,Map,Depth),Tp)
    | .cValof(Lc,Act,Tp) => valofAct(Lc,simplifyAct(Act,Map,Depth),Tp)
    | .cVarNme(Lc,Nm,V,E) where .cVar(_,_).=V =>
      .cVarNme(Lc,Nm,simplifyExp(V,Map,Depth),simplifyExp(E,Map,Depth))
    | .cVarNme(Lc,_,_,E) => simplifyExp(E,Map,Depth)
  }

  simCond:(cExp,map[defnSp,cDefn],integer) => cExp.
  simCond(.cCnj(Lc,L,R),Map,Depth) =>
    applyCnj(Lc,simCond(L,Map,Depth),simCond(R,Map,Depth)).
  simCond(.cDsj(Lc,L,R),Map,Depth) => applyDsj(Lc,simCond(L,Map,Depth),simCond(R,Map,Depth)).
  simCond(.cNeg(Lc,R),Map,Depth) => applyNeg(Lc,simExp(R,Map,Depth)).
  simCond(.cCnd(Lc,T,L,R),Map,Depth) =>
    applyCnd(Lc,simCond(T,Map,Depth),simCond(L,Map,Depth),simCond(R,Map,Depth),Map,Depth).
  simCond(.cMatch(Lc,Ptn,Exp),Map,Depth) =>
    applyMatch(Lc,simplifyExp(Ptn,Map,Depth),simplifyExp(Exp,Map,Depth),Map,Depth).
  simCond(C,Map,Depth) => simplifyExp(C,Map,Depth).

  valofAct(_,.aValis(_Lc,E),_) => E.
  valofAct(Lc,A,Tp) => .cValof(Lc,A,Tp).
  
  implementation simplify[aAction] => {
    simplify(A,Map,Dp) => simplifyAct(A,Map,Dp)
  }

  simplifyAct:(aAction,map[defnSp,cDefn],integer) => aAction.
  simplifyAct(A,P,D) => simAct(A,P,D).

  simAct(.aNop(Lc),_,_) => .aNop(Lc).
  simAct(.aSeq(Lc,.aDefn(DLc,.cVar(VLc,.cV(VNm,VTp)),E),A2),Map,Depth) where
      ~ isRefType(VTp) => valof{
    if traceInline! then{
      showMsg("attempt to inline var definition for #(VNm) = $(E)");
    };
    EE = simplifyExp(E,Map,Depth);
    AA = simplifyAct(A2,Map[.varSp(VNm)->.glDef(VLc,VNm,VTp,EE)],Depth);

    if ~present(AA,(Exp)=>(.cVar(_,.cV(VNm,_)).=Exp)) then
      valis AA
    else
    valis .aSeq(Lc,.aDefn(DLc,.cVar(VLc,.cV(VNm,VTp)),EE),AA)
  }
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
  simAct(.aDo(Lc,E),Map,Depth) => .aDo(Lc,simplifyExp(E,Map,Depth)).
  simAct(.aDefn(Lc,V,E),Map,Depth) =>
    .aDefn(Lc,simplifyExp(V,Map,Depth),simplifyExp(E,Map,Depth)).
  simAct(.aMatch(Lc,V,E),Map,Depth) =>
    .aMatch(Lc,simplifyExp(V,Map,Depth),simplifyExp(E,Map,Depth)).
  simAct(.aAsgn(Lc,V,E),Map,Depth) =>
    .aAsgn(Lc,simplifyExp(V,Map,Depth),simplifyExp(E,Map,Depth)).
  simAct(.aSetNth(Lc,T,Ix,E),Map,Depth) =>
    .aSetNth(Lc,simplifyExp(T,Map,Depth),Ix,simplifyExp(E,Map,Depth)).
  simAct(.aCase(Lc,Gov,Cases,Deflt),Map,Depth) =>
    inlineCase(Lc,simplifyExp(Gov,Map,Depth),Cases,simplifyAct(Deflt,Map,Depth),Map,Depth).
  simAct(.aIxCase(Lc,Gov,Cases,Deflt),Map,Depth) =>
    inlineIndex(Lc,simplifyExp(Gov,Map,Depth),Cases,simplifyAct(Deflt,Map,Depth),Map,Depth).
  simAct(.aIftte(Lc,T,L,R),Map,Depth) =>
    applyCnd(Lc,simplifyExp(T,Map,Depth),
      simplifyAct(L,Map,Depth-1),simplifyAct(R,Map,Depth-1),Map,Depth).
  simAct(.aWhile(Lc,T,A),Map,Depth) =>
    .aWhile(Lc,simplifyExp(T,Map,Depth),
      simplifyAct(A,Map,Depth)).
  simAct(.aTry(Lc,B,E,H),Map,Depth) where .cVar(VLc,EV) .= E => valof{
    NE = genVar(vName(EV),typeOf(EV));
    NH = simplifyAct(freshenA(H,{lName(EV)->.cVar(VLc,NE)}),Map,Depth);
    valis .aTry(Lc,simplifyAct(B,Map,Depth),.cVar(VLc,NE),NH)
  }
  simAct(.aThrw(Lc,E),Map,Depth) => .aThrw(Lc,simplifyExp(E,Map,Depth)).
  simAct(.aLtt(Lc,Vr,Bnd,A),Map,Depth) => valof{
    NE = genVar(vName(Vr),typeOf(Vr));
    valis inlineLtt(Lc,NE,simplifyExp(Bnd,Map,Depth),
      freshenA(A,{lName(Vr)->.cVar(Lc,NE)}), Map,Depth)
  }
  simAct(.aVarNme(Lc,Nm,Vr,X),Map,Depth) =>
    .aVarNme(Lc,Nm,simplifyExp(Vr,Map,Depth),simplifyAct(X,Map,Depth)).
  simAct(.aAbort(Lc,Txt),_,_) => .aAbort(Lc,Txt).

  dropNops(_,.aNop(_),A) => A.
  dropNops(_,A,.aNop(_)) => A.
  dropNops(Lc,L,R) => .aSeq(Lc,L,R).

  getValis(Lc,.cValof(_,A,_)) => A.
  getValis(Lc,E) => .aValis(Lc,E).
  
  inlineVar(Lc,.cV("_",Tp),_Map,_Depth) => .cAnon(Lc,Tp).
  inlineVar(Lc,.cV(Id,Tp),Map,Depth) where
      .glDef(_,_,_,Vl) ?= Map[.varSp(Id)] && isGround(Vl) => valof{
    Sim = simplify(Vl,Map,Depth);
    if traceInline! then
      showMsg("Replace global var #(Id) with $(Sim)");
    valis Sim
      }
  inlineVar(Lc,V,_,_) => .cVar(Lc,V).

  applyCnj(_,.cTerm(_,"true",[],_),R) => R.
  applyCnj(_,.cTerm(Lc,"false",[],Tp),R) => .cTerm(Lc,"false",[],Tp).
  applyCnj(Lc,L,R) => .cCnj(Lc,L,R).

  applyDsj(_,.cTerm(_,"false",[],_),R) => R.
  applyDsj(_,.cTerm(Lc,"true",[],Tp),R) => 
    .cTerm(Lc,"false",[],Tp).
  applyDsj(Lc,L,R) => .cDsj(Lc,L,R).

  applyNeg(_,.cTerm(Lc,"false",[],Tp)) => .cTerm(Lc,"true",[],Tp).
  applyNeg(_,.cTerm(Lc,"true",[],Tp)) => .cTerm(Lc,"false",[],Tp).
  applyNeg(Lc,Inner) => .cNeg(Lc,Inner).

  applyCnd:all e ~~ rewrite[e], reform[e] |= (option[locn],cExp,e,e,map[defnSp,cDefn],integer) => e.
  applyCnd(_,.cTerm(_,"false",[],_),_L,R,_,_) => R.
  applyCnd(_,.cTerm(_,"true",[],_),L,_R,_,_) => L.
  applyCnd(Lc,.cMatch(_,V,E),L,R,Map,Dep) where .cVar(_,.cV(Vr,_)) .= V =>
    rewrite(L,rwVar({Vr->E})).
  applyCnd(Lc,.cCnj(_,.cMatch(_,V,E),BB),L,R,Map,Dep) where .cVar(_,.cV(Vr,VTp)) .= V &&
      ~ varUsed(BB,.cV(Vr,VTp)) =>
    applyCnd(Lc,BB,rewrite(L,rwVar({Vr->E})),R,Map,Dep).
  applyCnd(Lc,T,L,R,_,_) => mkCond(Lc,T,L,R).

  inlineTplOff(_,.cTerm(_,_,Els,_),Ix,Tp) where E?=Els[Ix] => E.
  inlineTplOff(Lc,T,Ix,Tp) default => .cNth(Lc,T,Ix,Tp).
  
  applyTplUpdate(_,.cTerm(Lc,Nm,Args,Tp),Ix,E) =>
    .cTerm(Lc,Nm,Args[Ix->E],Tp).
  applyTplUpdate(Lc,T,Ix,E) =>
    .cSetNth(Lc,T,Ix,E).

  applyMatch(Lc,Ptn,Exp,_,_) where isGround(Ptn) && isGround(Exp) =>
    (Ptn==Exp ??
    .cTerm(Lc,"true",[],boolType) ||
    .cTerm(Lc,"false",[],boolType)).
  applyMatch(Lc,.cTerm(_,Lb,A1,_),.cTerm(_,Lb,A2,_),Map,Depth) =>
    makeSubMatches(Lc,A1,A2).
  applyMatch(Lc,Ptn,Exp,_,_) => .cMatch(Lc,Ptn,Exp).

  makeSubMatches(Lc,[],[]) => .cTerm(Lc,"true",[],boolType).
  makeSubMatches(Lc,[T1],[T2]) => .cMatch(Lc,T1,T2).
  makeSubMatches(Lc,[P1,..P1s],[T2,..T2s]) => .cCnj(Lc,.cMatch(Lc,P1,T2),makeSubMatches(Lc,P1s,T2s)).

  simplifyCase:all e ~~ rewrite[e], reform[e], simplify[e] |=
    (cCase[e],map[defnSp,cDefn],integer) => cCase[e].
  simplifyCase((Lc,Ptn,Rep),Map,Dp) => (Lc,Ptn,simplify(Rep,Map,Dp)).

  isSingletonType:(string,map[defnSp,cDefn])=>boolean.
  isSingletonType(Nm,Map) where .tpDef(_,_,_,CMp)?=Map[.tpSp(Nm)] => [|CMp|]==1.
  isSingletonType(_,_) default => .false.

  inlineCase:all e ~~ rewrite[e], reform[e], simplify[e] |=
    (option[locn],cExp,cons[cCase[e]],e,map[defnSp,cDefn],integer) => e.
  inlineCase(Lc,Gov,Cases,Deflt,Map,Depth) where
      .matching(Exp) .= matchingCase(Gov,Cases,Map,Depth) => Exp.
  inlineCase(Lc,Gov,Cases,Deflt,Map,Depth) =>
    mkCase(Lc,Gov,Cases,Deflt).

  inlineIndex:all e ~~ rewrite[e], reform[e], simplify[e], display[e] |=
    (option[locn],cExp,cons[cCase[e]],e,map[defnSp,cDefn],integer) => e.
  inlineIndex(Lc,Gov,Cases,Deflt,Map,Depth) where
      .matching(Exp) .= matchingCase(Gov,Cases,Map,Depth) => Exp.
  inlineIndex(Lc,Gov,Cases,Deflt,Map,Depth) =>
    mkIndex(Lc,Gov,Cases,Deflt).

  matchingCase:all e ~~ rewrite[e], reform[e], simplify[e] |=
    (cExp,cons[cCase[e]],map[defnSp,cDefn],integer) => match[e].
  matchingCase(_,[],_,_) => .noMatch.
  matchingCase(Gov,[C,..Cs],Map,Depth) => case candidate(Gov,C) in {
    | .insufficient => .insufficient
    | .noMatch => matchingCase(Gov,Cs,Map,Depth)
    | .matching(Rep) => .matching(simplify(Rep,Map,Depth))
  }.

  candidate:all e ~~ rewrite[e] |= (cExp,cCase[e]) => match[e].
  candidate(E,(_,Ptn,Rep)) => case ptnMatch(Ptn,E,[]) in {
    | .matching(Theta) => .matching(rewrite(Rep,rwVar(Theta)))
    | .noMatch => .noMatch
    | .insufficient => .insufficient
  }

  inlineLtt:all e ~~ simplify[e],reform[e],present[e],rewrite[e] |=
    (option[locn],cV,cExp,e,map[defnSp,cDefn],integer) => e.
  inlineLtt(Lc,.cV(Vr,Tp),Bnd,Exp,Map,Depth) where isGround(Bnd) =>
    simplify(rewrite(Exp,rwVar({Vr->Bnd})),Map,Depth).
  inlineLtt(Lc,Vr,Bnd,Exp,Map,Depth) =>
    mkLtt(Lc,Vr,Bnd,simplify(Exp,Map,Depth)).

  varFound(Vr) => (T)=>(.cVar(_,VV).=T ?? VV==Vr || .false).
  
  /* Specializing a generic function's body for one call site, requires
  propagating type information into the function from the call site.  */

  -- freshen only opens the outermost quantifier; access functions (see
  -- parsetype.star's makeAccessor) nest another after an arrow. Open that
  -- one too, but only if CallTp shows it as applied -- if CallTp is also
  -- quantified there it's genuinely still polymorphic, and sameType already
  -- matches quantified-vs-quantified.
  alignQuants:(tipe,tipe,dict) => (cons[(string,tipe)],tipe).
  alignQuants(Tp,CallTp,Dict) => valof{
    (Q1,Tp1) = freshen(Tp,Dict);
    valis alignFurther(Q1,Tp1,CallTp,Dict)
  }

  alignFurther:(cons[(string,tipe)],tipe,tipe,dict) => (cons[(string,tipe)],tipe).
  alignFurther(Q1,.funType(A,R,E),CallTp,Dict)
      where .allType(_,_).=deRef(R) && .funType(_,CR,_).=deRef(CallTp) && isNotAllType(deRef(CR)) => valof{
    (Q2,R1) = alignQuants(R,CR,Dict);
    valis (Q1++Q2,.funType(A,R1,E))
  }
  alignFurther(Q1,Tp1,_,_) default => (Q1,Tp1).

  isNotAllType(.allType(_,_)) => .false.
  isNotAllType(_) default => .true.

  specializeTypes:(tipe,tipe,cons[(string,tipe)],cExp,option[locn],string) => cExp.
  specializeTypes(FreshFTp,CallTp,Quants,Rep,Lc,Nm) =>
    sameType(FreshFTp,CallTp,emptyDict) ??
      substTypesE(Rep,Quants)
    || valof{
      reportError("cannot align types when inlining #(Nm)\: $(FreshFTp) vs $(CallTp)",Lc);
      valis Rep
    }.

  substTp:(tipe,cons[(string,tipe)]) => tipe.
  substTp(Tp,Quants) => refresh(Quants,Tp,emptyDict).

  substCV:(cV,cons[(string,tipe)]) => cV.
  substCV(.cV(Nm,Tp),Quants) => .cV(Nm,substTp(Tp,Quants)).

  public substTypesE:(cExp,cons[(string,tipe)]) => cExp.
  substTypesE(E,Quants) => foldExp(E,Quants,substTypesAlgebra).

  public substTypesA:(aAction,cons[(string,tipe)]) => aAction.
  substTypesA(A,Quants) => foldAct(A,Quants,substTypesAlgebra).

  substCasesTpE:(cons[cCase[cExp]],cons[(string,tipe)]) => cons[cCase[cExp]].
  substCasesTpE(Cs,Quants) =>
    (Cs//((Lc,Ptn,Rep))=>(Lc,foldExp(Ptn,Quants,substTypesAlgebra),foldExp(Rep,Quants,substTypesAlgebra))).

  substCasesTpA:(cons[cCase[aAction]],cons[(string,tipe)]) => cons[cCase[aAction]].
  substCasesTpA(Cs,Quants) =>
    (Cs//((Lc,Ptn,Rep))=>(Lc,foldExp(Ptn,Quants,substTypesAlgebra),foldAct(Rep,Quants,substTypesAlgebra))).

  /* Only the fields that actually carry a tipe (or a cV, which carries one) do
  anything here.  */

  substTypesAlgebra:treeAlgebra[cons[(string,tipe)],cExp,aAction].
  substTypesAlgebra = treeAlgebra{
    onVoid(_,Lc)=>.cVoid(Lc).
    onAnon(Q,Lc,Tp)=>.cAnon(Lc,substTp(Tp,Q)).
    onUnrch(Q,Lc,Tp)=>.cUnrch(Lc,substTp(Tp,Q)).
    onVar(Q,Lc,V)=>.cVar(Lc,substCV(V,Q)).
    onCel(Q,Lc,E,Tp)=>.cCel(Lc,E,substTp(Tp,Q)).
    onGet(Q,Lc,E,Tp)=>.cGet(Lc,E,substTp(Tp,Q)).
    onInt(_,Lc,Ix)=>.cInt(Lc,Ix).
    onChar(_,Lc,Cx)=>.cChar(Lc,Cx).
    onBig(_,Lc,Ix)=>.cBig(Lc,Ix).
    onFlt(_,Lc,Dx)=>.cFlt(Lc,Dx).
    onString(_,Lc,Sx)=>.cString(Lc,Sx).
    onTerm(Q,Lc,Op,Args,Tp)=>.cTerm(Lc,Op,Args,substTp(Tp,Q)).
    onNth(Q,Lc,R,Ix,Tp)=>.cNth(Lc,R,Ix,substTp(Tp,Q)).
    onSetNth(_,Lc,R,Ix,E)=>.cSetNth(Lc,R,Ix,E).
    onClos(Q,Lc,L,A,F,Tp)=>.cClos(Lc,L,A,F,substTp(Tp,Q)).
    onSv(Q,Lc,Tp)=>.cSv(Lc,substTp(Tp,Q)).
    onSvDrf(Q,Lc,E,Tp)=>.cSvDrf(Lc,E,substTp(Tp,Q)).
    onSvSet(_,Lc,E,V)=>.cSvSet(Lc,E,V).
    onCall(Q,Lc,Op,Args,Tp)=>.cCall(Lc,Op,Args,substTp(Tp,Q)).
    onOCall(Q,Lc,Op,Args,Tp)=>.cOCall(Lc,Op,Args,substTp(Tp,Q)).
    onXCall(Q,Lc,Op,Args,Tp,ErTp)=>.cXCall(Lc,Op,Args,substTp(Tp,Q),substTp(ErTp,Q)).
    onXOCall(Q,Lc,Op,Args,Tp,ErTp)=>.cXOCall(Lc,Op,Args,substTp(Tp,Q),substTp(ErTp,Q)).
    onSeq(_,Lc,L,R)=>.cSeq(Lc,L,R).
    onCnj(_,Lc,L,R)=>.cCnj(Lc,L,R).
    onDsj(_,Lc,L,R)=>.cDsj(Lc,L,R).
    onNeg(_,Lc,R)=>.cNeg(Lc,R).
    onCnd(_,Lc,G,L,R)=>.cCnd(Lc,G,L,R).
    onLtt(Q,Lc,V,D,E)=>.cLtt(Lc,substCV(V,Q),D,E).
    onCase(Q,Lc,Sel,Cases,Dflt,Tp)=>.cCase(Lc,Sel,substCasesTpE(Cases,Q),Dflt,substTp(Tp,Q)).
    onIxCase(Q,Lc,Sel,Cases,Dflt,Tp)=>.cIxCase(Lc,Sel,substCasesTpE(Cases,Q),Dflt,substTp(Tp,Q)).
    onMatch(_,Lc,P,E)=>.cMatch(Lc,P,E).
    onResum(Q,Lc,T,M,Tp)=>.cResum(Lc,T,M,substTp(Tp,Q)).
    onSusp(Q,Lc,T,M,Tp)=>.cSusp(Lc,T,M,substTp(Tp,Q)).
    onRetyr(Q,Lc,T,M,Tp)=>.cRetyr(Lc,T,M,substTp(Tp,Q)).
    onVarNme(_,Lc,N,V,E)=>.cVarNme(Lc,N,V,E).
    onAbort(Q,Lc,Ms,Tp)=>.cAbort(Lc,Ms,substTp(Tp,Q)).
    onTry(Q,Lc,B,E,H,Tp)=>.cTry(Lc,B,E,H,substTp(Tp,Q)).
    onThrw(Q,Lc,E,Tp)=>.cThrw(Lc,E,substTp(Tp,Q)).
    onValof(Q,Lc,A,Tp)=>.cValof(Lc,A,substTp(Tp,Q)).

    onANop(_,Lc)=>.aNop(Lc).
    onASeq(_,Lc,L,R)=>.aSeq(Lc,L,R).
    onALbld(_,Lc,L,A)=>.aLbld(Lc,L,A).
    onABreak(_,Lc,L)=>.aBreak(Lc,L).
    onAValis(_,Lc,E)=>.aValis(Lc,E).
    onADo(_,Lc,E)=>.aDo(Lc,E).
    onASetNth(_,Lc,V,Ix,E)=>.aSetNth(Lc,V,Ix,E).
    onADefn(_,Lc,V,E)=>.aDefn(Lc,V,E).
    onAMatch(_,Lc,V,E)=>.aMatch(Lc,V,E).
    onAAsgn(_,Lc,V,E)=>.aAsgn(Lc,V,E).
    onACase(Q,Lc,G,Cs,D)=>.aCase(Lc,G,substCasesTpA(Cs,Q),D).
    onAIxCase(Q,Lc,G,Cs,D)=>.aIxCase(Lc,G,substCasesTpA(Cs,Q),D).
    onAIftte(_,Lc,C,L,R)=>.aIftte(Lc,C,L,R).
    onAWhile(_,Lc,C,B)=>.aWhile(Lc,C,B).
    onATry(_,Lc,B,E,Hs)=>.aTry(Lc,B,E,Hs).
    onAThrw(_,Lc,E)=>.aThrw(Lc,E).
    onALtt(Q,Lc,V,D,A)=>.aLtt(Lc,substCV(V,Q),D,A).
    onAVarNme(_,Lc,N,V,E)=>.aVarNme(Lc,N,V,E).
    onAAbort(_,Lc,Ms)=>.aAbort(Lc,Ms).

    extendLtt(Q,_)=>Q.
    onRaw(_,_)=>.none.
    onARaw(_,_)=>.none.
    }.

  inlineCall:(option[locn],string,cons[cExp],tipe,map[defnSp,cDefn],integer) => cExp.
  inlineCall(Lc,Nm,Args,Tp,Map,Depth) where Depth>0 &&
      Def ?= Map[.varSp(Nm)] && .fnDef(_,_,FTp,Vrs,Rep).=Def && isSmall(Def) =>
    valof{
    CallTp = .funType(.tupleType(Args//typeOf),Tp,.voidType);
    (Quants,FreshFTp) = alignQuants(FTp,CallTp,emptyDict);
    if traceInline! then{
      showMsg("Inlining call to #(Nm)\:$(FTp) @ $(Lc), expecting $(CallTp)");
      showMsg("Freshened callee type $(FreshFTp)");
    }

    SpecRep = specializeTypes(FreshFTp,CallTp,Quants,Rep,Lc,Nm);

    RwMap = { lName(V)->A | (V,A) in zip(Vrs,Args)};

    inlined = simplifyExp(freshenE(SpecRep,RwMap),Map[~.varSp(Nm)],Depth-1);

    if traceInline! then{
      showMsg("Inlined call to #(Nm) is $(inlined)")
    };

    valis inlined
    }.
  inlineCall(Lc,Nm,Args,Tp,Map,Depth) default => .cCall(Lc,Nm,Args//(A)=>simExp(A,Map,Depth),Tp).

  inlineXCall:(option[locn],string,cons[cExp],tipe,tipe,map[defnSp,cDefn],integer) => cExp.
  inlineXCall(Lc,Nm,Args,Tp,ErTp,Map,Depth) where Depth>0 &&
      Def ?= Map[.varSp(Nm)] && .fnDef(_,_,FTp,Vrs,Rep).=Def && isSmall(Def) => valof{
    CallTp = .funType(.tupleType(Args//typeOf),Tp,ErTp);
    (Quants,FreshFTp) = alignQuants(FTp,CallTp,emptyDict);

    if traceInline! then
      showMsg("Inlining xcall to #(Nm)\:$(FTp)");

    SpecRep = specializeTypes(FreshFTp,CallTp,Quants,Rep,Lc,Nm);

    RwMap = { lName(V)->A | (V,A) in zip(Vrs,Args)};
    valis simplifyExp(freshenE(SpecRep,RwMap),Map[~.varSp(Nm)],Depth-1)
      }.
  inlineXCall(Lc,Nm,Args,Tp,ErTp,Map,Depth) default => .cXCall(Lc,Nm,Args//(A)=>simExp(A,Map,Depth),Tp,ErTp).
  inlineECall:(option[locn],string,cons[cExp],tipe,integer) => cExp.
  inlineECall(Lc,Nm,Args,Tp,Depth) where Depth>0 && {? A in Args *> isGround(A) ?} =>
    rewriteECall(Lc,Nm,Args,Tp).
  inlineECall(Lc,Nm,Args,Tp,_) default => .cCall(Lc,Nm,Args,Tp).

  inlineOCall(Lc,.cTerm(OLc,Nm,OArgs,ATp),Args,Tp,Map,Depth) =>
    simplifyExp(.cCall(Lc,Nm,[.cTerm(OLc,Nm,OArgs,ATp),..Args],Tp),Map,Depth).
  inlineOCall(Lc,.cClos(OLc,Nm,_,Fr,_),Args,Tp,Map,Depth) =>
    simplifyExp(.cCall(Lc,Nm,[Fr,..Args],Tp),Map,Depth).
  inlineOCall(Lc,Op,Args,Tp,Map,Depth) => .cOCall(Lc,Op,Args//(A)=>simExp(A,Map,Depth),Tp).
  
  inlineXOCall(Lc,.cTerm(OLc,Nm,OArgs,ATp),Args,Tp,ErTp,Map,Depth) =>
    simplifyExp(.cXCall(Lc,Nm,[.cTerm(OLc,Nm,OArgs,ATp),..Args],Tp,ErTp),Map,Depth).
  inlineXOCall(Lc,.cClos(OLc,Nm,_,Fr,_),Args,Tp,ErTp,Map,Depth) =>
    simplifyExp(.cXCall(Lc,Nm,[Fr,..Args],Tp,ErTp),Map,Depth).
  inlineXOCall(Lc,Op,Args,Tp,ErTp,Map,Depth) => .cXOCall(Lc,Op,Args//(A)=>simExp(A,Map,Depth),Tp,ErTp).

  rewriteECall(Lc,"_int_plus",[.cInt(_,A),.cInt(_,B)],_) => .cInt(Lc,A+B).
  rewriteECall(Lc,"_int_minus",[.cInt(_,A),.cInt(_,B)],_) => .cInt(Lc,A-B).
  rewriteECall(Lc,"_int_times",[.cInt(_,A),.cInt(_,B)],_) => .cInt(Lc,A*B).
  rewriteECall(Lc,"_int_eq",[.cInt(_,A),.cInt(_,B)],_) =>
    .cTerm(Lc,(A == B??"true"||"false"),[],boolType).
  rewriteECall(Lc,"_int_lt",[.cInt(_,A),.cInt(_,B)],_) =>
    .cTerm(Lc,(A < B??"true"||"false"),[],boolType).
  rewriteECall(Lc,"_int_ge",[.cInt(_,A),.cInt(_,B)],_) =>
    .cTerm(Lc,(A >= B??"true"||"false"),[],boolType).

  rewriteECall(Lc,"_flt_plus",[.cFlt(_,A),.cFlt(_,B)],_) => .cFlt(Lc,A+B).
  rewriteECall(Lc,"_flt_minus",[.cFlt(_,A),.cFlt(_,B)],_) => .cFlt(Lc,A-B).
  rewriteECall(Lc,"_flt_times",[.cFlt(_,A),.cFlt(_,B)],_) => .cFlt(Lc,A*B).
  rewriteECall(Lc,"_flt_eq",[.cFlt(_,A),.cFlt(_,B)],_) =>
    .cTerm(Lc,(A == B??"true"||"false"),[],boolType).
  rewriteECall(Lc,"_flt_lt",[.cFlt(_,A),.cFlt(_,B)],_) =>
    .cTerm(Lc,(A < B??"true"||"false"),[],boolType).
  rewriteECall(Lc,"_flt_ge",[.cFlt(_,A),.cFlt(_,B)],_) =>
    .cTerm(Lc,(A >= B??"true"||"false"),[],boolType).

  rewriteECall(Lc,"_str_multicat",[As],_) where isGround(As) =>
    .cString(Lc,pullStrings(As)*).
  rewriteECall(Lc,Op,Args,Tp) default => .cCall(Lc,Op,Args,Tp).

  pullStrings(.cTerm(_,"nil",[],_)) => [].
  pullStrings(.cTerm(_,"cons",[.cString(_,S),Tl],_)) => [S,..pullStrings(Tl)].

  isLeafDef:(cDefn) => boolean.
  isLeafDef(D) => ~present(D,(Cll) =>
      (.cOCall(_,_,_,_).=Cll ||
      (.cCall(_,Nm,_,_) .= Cll && ~isEscape(Nm)))).

  isSmall:(cDefn) => boolean.
  isSmall(Df) => isLeafDef(Df) || [|Df|] < 32.
}
