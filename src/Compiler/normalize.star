star.compiler.normalize{
  import star.
  import star.pkg.
  import star.sort.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.term.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.freevars.
  import star.compiler.matcher.
  import star.compiler.meta.
  import star.compiler.normalize.meta.
  import star.compiler.misc.
  import star.compiler.types.

  import star.compiler.location.
  import star.compiler.data.

  public normalize:(pkgSpec,cons[canonDef],cons[decl])=>cons[cDefn].
  normalize(PkgSpec,Defs,Decls) => valof{
    Map = pkgMap(Decls,pkgMap(stdTypes,[]));
    valis transformGroup(Defs,Map,Map,[],.none,[])
  }

  transformGroup:(cons[canonDef],nameMap,nameMap,set[cV],option[cExp],cons[cDefn]) =>
    cons[cDefn].
  transformGroup([],_,_,_,_,D) => D.
  transformGroup([D,..Ds],Map,Outer,Q,Extra,Ex) => valof {
    Ex1 = transformDef(D,Map,Outer,Q,Extra,Ex);
    valis transformGroup(Ds,Map,Outer,Q,Extra,Ex1)
  }

  all e ~~ crFlow[e] ~> (e,cons[cDefn]).

  transformDef:(canonDef,nameMap,nameMap,set[cV],option[cExp],cons[cDefn]) => cons[cDefn].
  transformDef(.funDef(Lc,FullNm,Eqns,_,Tp),Map,Outer,Q,Extra,Ex) =>
    transformFunction(Lc,FullNm,Eqns,Tp,Map,Outer,Q,Extra,Ex).
  transformDef(.varDef(Lc,_,FullNm,.lambda(_,LNm,Eqn,Tp),_,_),Map,Outer,Q,Extra,Ex) =>
    transformFunction(Lc,FullNm,[Eqn],Tp,Map,Outer,Q,Extra,Ex).
  transformDef(.varDef(Lc,_,FullNm,Val,Cx,Tp),Map,Outer,Q,.none,Ex) => valof{
    (Vl,Defs) = liftExp(Val,Outer,Q,Ex);
    valis [.glDef(Lc,FullNm,Tp,Vl),..Defs]
  }
  transformDef(.varDef(Lc,_,FullNm,Val,_,Tp),Map,Outer,Q,Extra,Ex) =>
    transformFunction(Lc,FullNm,[.rule(Lc,.tple(Lc,[]),.none,Val)],funType([],Tp),Map,Outer,Q,Extra,Ex).
  transformDef(.implDef(Lc,Nm,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Ex) =>
    transformDef(.varDef(Lc,Nm,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Ex).
  transformDef(.typeDef(Lc,Nm,Tp,TpRl),Map,_,_,_,Ex) =>
    transformTypeDef(Lc,Nm,Tp,TpRl,Map,Ex).
  transformDef(.cnsDef(Lc,FullNm,Ix,Tp),Map,_,_,_,Ex) =>
    transformConsDef(Lc,FullNm,Ix,Tp,Map,Ex).

  transformFunction(Lc,FullNm,Eqns,Tp,Map,Outer,Q,Extra,Ex) => valof{
    if traceNormalize! then{
      showMsg("transform function $(.funDef(Lc,FullNm,Eqns,[],Tp)) @ $(Lc)");
    };
    ATp = extendFunTp(deRef(Tp),Extra);
    if traceNormalize! then
      showMsg("extended function function type $(ATp)");
    
    (Eqs,Ex1) = transformRules(Eqns,Map,Outer,Q,Extra,Ex);
    if traceNormalize! then
      showMsg("transformed equations: $(Eqs)");
    try{
      Func = ? functionMatcher(Lc,FullNm,ATp,Map,Eqs);
      if traceNormalize! then
	showMsg("transformed function $(Func)");

      ClosureNm = closureNm(FullNm);
      ClVar = (.cVar(_,Exv)?=Extra ?? Exv || .cV("_",unitTp));
      ClVars = makeFunVars(Tp);
      ClArgs = ([ClVar,..ClVars]//(V)=>.cVar(Lc,V));

      ClosTp = extendFunTp(deRef(Tp),.some(ClVar));

      if Exv?=Extra then {
	ClosEntry =
	  .fnDef(Lc,ClosureNm,ClosTp,ClArgs,.cCall(Lc,FullNm,ClArgs,funTypeRes(Tp)));
	valis [Func,ClosEntry,..Ex1]
      } else {
	ClosEntry =
	  .fnDef(Lc,ClosureNm,ClosTp,
	  ClArgs,.cCall(Lc,FullNm,ClVars//(V)=>.cVar(Lc,V),funTypeRes(Tp)));
	valis [Func,ClosEntry,..Ex1]
      }
    } catch {
      _ => {
	reportError("cannot build matcher for function $(FullNm)",Lc);
	valis Ex
      }
    }
  }

  transformTypeDef(Lc,Nm,Tp,TpRl,Map,Ex) => valof{
    if ConsMap ?= findIndexMap(Nm,Map) then
      valis [.tpDef(Lc,Tp,TpRl,ConsMap),..Ex]
    else
    valis [.tpDef(Lc,Tp,TpRl,[]),..Ex]
  }

  transformConsDef(Lc,Nm,Ix,Tp,Map,Ex) => 
    [.lblDef(Lc,.tLbl(Nm,arity(Tp)),Tp,Ix),..Ex].

  contract all e,t ~~ transform[e->>t] ::= {
    transform:(e,nameMap,set[cV],cons[cDefn]) => (t,cons[cDefn]).
  }

  implementation transform[canon->>cExp] => {
    transform(E,Map,Q,Ex) => liftExp(E,Map,Q,Ex).
  }

  implementation transform[canonAction->>aAction] => {
    transform(A,Map,Q,Ex) => liftAction(A,Map,Q,Ex).
  }

  contract all t ~~ letify[t] ::= {
    letify:(option[locn],cV,cExp,t) => t.
    freeUpdate:(option[locn],cExp,integer,cExp,t) => t.
  }

  implementation letify[cExp] => {
    letify(Lc,V,Vl,B) => .cLtt(Lc,V,Vl,B).
    freeUpdate(Lc,Vr,Ix,Vl,SoFar) => .cSeq(Lc,.cSetNth(Lc,Vr,Ix,Vl),SoFar).
  }

  implementation letify[aAction] => {
    letify(Lc,V,Vl,B) => .aLtt(Lc,V,Vl,B).
    freeUpdate(Lc,Vr,Ix,Vl,SoFar) => .aSeq(Lc,.aSetNth(Lc,Vr,Ix,Vl),SoFar).
  }

  transformRules:all e,t ~~ transform[e->>t], display[e], display[t] |:
    (cons[rule[e]],nameMap,nameMap,set[cV],option[cExp],cons[cDefn]) =>
      (cons[(option[locn],cons[cExp],option[cExp],t)],cons[cDefn]).
  transformRules([],_,_,_,_,Ex) => ([],Ex).
  transformRules([Eqn,..Eqns],Map,Outer,Q,Extra,Ex) => valof{
    (Trple,Ex1) = transformRule(Eqn,Map,Outer,Q,Extra,Ex);
    (Rest,Exx) = transformRules(Eqns,Map,Outer,Q,Extra,Ex1);
    valis ([Trple,..Rest],Exx)
  }

  transformRule:all e,t ~~ transform[e->>t], display[e], display[t]|:
    (rule[e],nameMap,nameMap,set[cV],option[cExp],cons[cDefn]) =>
      ((option[locn],cons[cExp],option[cExp],t),cons[cDefn]).
  transformRule(.rule(Lc,Arg,Test,Val),Map,Outer,Q,Extra,Ex) => valof{
    EQ = ptnVars(Arg,Q,[]);
    if traceNormalize! then
      showMsg("Pattern vars $(EQ)");
    (APtn,Ex1) = liftPtn(Arg,Outer,EQ,Ex);

    if traceNormalize! then
      showMsg("lifted pattern $(APtn)");

    (TPtn, WC) = pullWhere(APtn);

    GEQ = (Tst?=Test ?? condVars(Tst,EQ) || EQ);
    (NG,Ex2) = liftGoal(Test,Map,GEQ,Ex1);
    (Rep,Exx) = transform(Val,Map,GEQ,Ex2);
    if traceNormalize! then
      showMsg("Val $(Val) lifted to $(Rep)");
    
    if .cTerm(_,_,Ptns,_).=TPtn then
      valis ((Lc,addExtra(Extra,Ptns),mergeGoal(Lc,WC,NG),Rep),Exx)
    else{
      reportError("cannot transform invalid rule args $(Arg)",Lc);
      valis ((Lc,addExtra(Extra,[]),.none,Rep),Exx)
    }
  }

  liftGoal(.none,_,_,Ex) => (.none,Ex).
  liftGoal(.some(C),Map,Q,Ex) => valof{
    (C1,Ex1) = liftExp(C,Map,Q,Ex);
    valis (.some(C1),Ex1)
  }
  
  addExtra(.none,Args) => Args.
  addExtra(.some(P),Args) => [P,..Args].

  liftPtn:(canon,nameMap,set[cV],cons[cDefn]) => crFlow[cExp].
  liftPtn(.anon(Lc,Tp),Map,_,Ex) => (.cAnon(Lc,Tp),Ex).
  liftPtn(.vr(Lc,Nm,Tp),Map,Q,Ex) => trVarPtn(Lc,Nm,Tp,Map,Q,Ex).
  liftPtn(.enm(Lc,Nm,Tp),Map,Q,Ex) => liftPtnCallOp(Lc,Nm,[],Tp,Map,Q,Ex).
  liftPtn(.intr(Lc,Ix),Map,_,Ex) =>  (.cInt(Lc,Ix),Ex).
  liftPtn(.bintr(Lc,Ix),Map,_,Ex) => (.cBig(Lc,Ix),Ex).
  liftPtn(.flt(Lc,Dx),Map,_,Ex) => (.cFlt(Lc,Dx),Ex).
  liftPtn(.kar(Lc,Cx),Map,_,Ex) => (.cChar(Lc,Cx),Ex).
  liftPtn(.strng(Lc,Sx),Map,_,Ex) => (.cString(Lc,Sx),Ex).
  liftPtn(.tple(Lc,Els),Map,Q,Ex) => valof{
    (LEls,Exx) = liftPtns(Els,Map,Q,Ex);
    valis (crTpl(Lc,LEls),Exx)
  }
  liftPtn(.apply(Lc,.vr(VLc,VNm,_),Els,Tp),Map,Q,Ex) => valof{
    (LArgs,Ex1) = liftPtns(Els,Map,Q,Ex);
    valis liftPtnCallOp(Lc,VNm,LArgs,Tp,Map,Q,Ex1)
  }
  liftPtn(.apply(Lc,.enm(VLc,Nm,ETp),Els,Tp),Map,Q,Ex) => valof{
    (LArgs,Ex1) = liftPtns(Els,Map,Q,Ex);

    valis liftPtnCallOp(Lc,Nm,LArgs,Tp,Map,Q,Ex1)
  }
  liftPtn(.svGet(Lc,S,Tp),Map,Q,Ex) => valof{
    (SS,Ex1) = liftExp(S,Map,Q,Ex);
    valis (.cSvDrf(Lc,SS,Tp),Ex1)
  }
  liftPtn(Cn,_,_,Ex) => valof{
    Lc = locOf(Cn);
    reportError("may not have $(Cn) as a pattern",Lc);
    valis (.cVoid(Lc,typeOf(Cn)),Ex)
  }
  
  liftPtns:(cons[canon],nameMap,set[cV],cons[cDefn]) => (cons[cExp],cons[cDefn]).
  liftPtns([],_,_,Ex) => ([],Ex).
  liftPtns([P,..Ps],Map,Q,Ex) => valof{
    (A,Ex1) = liftPtn(P,Map,Q,Ex);
    (As,Exx) = liftPtns(Ps,Map,Q,Ex1);
    valis ([A,..As],Exx)
  }

  liftPtnCallOp:(option[locn],string,cons[cExp],tipe,nameMap,set[cV],cons[cDefn]) =>
    (cExp,cons[cDefn]).
  liftPtnCallOp(Lc,Nm,Args,Tp,Map,Q,Ex) where Entry ?= lookupVarName(Map,Nm) =>
    implementPtnCall(Lc,Entry,Args,Tp,Map,Q,Ex).

  implementPtnCall(Lc,.moduleCons(Nm,CTp),Args,Tp,_,_,Ex) =>
    (.cTerm(Lc,Nm,Args,Tp),Ex).
  implementPtnCall(Lc,.localCons(Nm,CTp,Vr),Args,Tp,_,_,Ex) =>
    (.cTerm(Lc,Nm,[.cVar(Lc,Vr),..Args],Tp),Ex).
  
  trVarPtn(Lc,Nm,Tp,Map,Q,Ex) => ({? .cV(Nm,Tp) in Q ?} ??
    (.cVar(Lc,.cV(Nm,Tp)),Ex) ||
    implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex)).

  implementVarPtn(Lc,Nm,.none,Tp,_,Ex) => (.cVar(Lc,.cV(Nm,Tp)),Ex).
  implementVarPtn(Lc,Nm,.some(.moduleCons(Enum,CTp)),Tp,_,Ex) where ETp?=isEnumType(CTp) =>
    (.cTerm(Lc,Enum,[],ETp),Ex).
  implementVarPtn(Lc,Nm,.some(.localCons(Enum,CTp,Vr)),Tp,_,Ex) =>
    (.cTerm(Lc,Enum,[.cVar(Lc,Vr)],Tp),Ex).
  implementVarPtn(Lc,Nm,.some(V),Tp,_Map,Ex) => valof{
    reportError("not permitted to match against $(Nm)\:$(V)",Lc);
    valis (.cVoid(Lc,Tp),Ex)
  }.

  liftExp:(canon,nameMap,set[cV],cons[cDefn]) => crFlow[cExp].
  liftExp(.anon(Lc,Tp),Map,Q,Ex) => (.cAnon(Lc,Tp),Ex).
  liftExp(.vr(Lc,Nm,Tp),Map,Q,Ex) => valof{
    VV = liftVarExp(Lc,Nm,Tp,Map);
    valis (VV,Ex)
  }
  liftExp(.intr(Lc,Ix),Map,_,Ex) => (.cInt(Lc,Ix),Ex).
  liftExp(.bintr(Lc,Ix),Map,_,Ex) => (.cBig(Lc,Ix),Ex).
  liftExp(.flt(Lc,Dx),Map,_,Ex) => (.cFlt(Lc,Dx),Ex).
  liftExp(.kar(Lc,Cx),Map,_,Ex) => (.cChar(Lc,Cx),Ex).
  liftExp(.strng(Lc,Sx),Map,_,Ex) => (.cString(Lc,Sx),Ex).
  liftExp(.enm(Lc,Nm,Tp),Map,_,Ex) => valof{
    VV = liftVarExp(Lc,Nm,Tp,Map);
    valis (VV,Ex)
  }
  liftExp(.tple(Lc,Els),Map,Q,Ex) => valof{
    (LEls,Exx) = liftExps(Els,Q,Map,Ex);
    valis (crTpl(Lc,LEls),Exx)
  }
  liftExp(.apply(Lc,Op,Els,Tp),Map,Q,Ex) => valof{
    (LEls,Ex1) = liftExps(Els,Q,Map,Ex);
    valis liftExpCallOp(Lc,Op,LEls,Tp,Map,Q,Ex1)
  }
  liftExp(.dot(Lc,Rc,Fld,Tp),Map,Q,Ex) => valof{
    reportError("unexpected dot expression $(.dot(Lc,Rc,Fld,Tp))",Lc);
    valis (.cVoid(Lc,Tp),Ex)
  }
  liftExp(.tdot(Lc,Rc,Ix,Tp),Map,Q,Ex) => valof{
    (LRc,Ex1) = liftExp(Rc,Map,Q,Ex);
    valis (.cNth(Lc,LRc,Ix,Tp),Ex1)
  }
  liftExp(.conj(Lc,L,R),Map,Q,Ex) => valof{
    (LL,Ex1) = liftExp(L,Map,Q,Ex);
    (LR,Ex2) = liftExp(R,Map,condVars(L,Q),Ex1);
    valis (.cCnj(Lc,LL,LR),Ex2)
  }
  liftExp(.disj(Lc,L,R),Map,Q,Ex) => valof{
    (LL,Ex1) = liftExp(L,Map,Q,Ex);
    (LR,Ex2) = liftExp(R,Map,Q,Ex1);
    valis (.cDsj(Lc,LL,LR),Ex2)
  }
  liftExp(.neg(Lc,R),Map,Q,Ex) => valof{
    (LR,Ex1) = liftExp(R,Map,Q,Ex);
    valis (.cNeg(Lc,LR),Ex1)
  }
  liftExp(.cond(Lc,T,L,R),Map,Q,Ex) => valof{
    (LT,Ex1) = liftExp(T,Map,Q,Ex);
    (LL,Ex2) = liftExp(L,Map,condVars(T,Q),Ex1);
    (LR,Ex3) = liftExp(R,Map,Q,Ex2);

    valis (.cCnd(Lc,LT,LL,LR),Ex3)
  }
  liftExp(.match(Lc,P,E),Map,Q,Ex) => valof{
    (LP,Ex1) = liftPtn(P,Map,Q,Ex);
    (LE,Ex2) = liftExp(E,Map,Q,Ex1);
    valis (.cMatch(Lc,LP,LE),Ex2)
  }
  liftExp(.letExp(Lc,Grp,Decs,Bnd),Map,Q,Ex) => valof{
    Free = findFree(.letExp(Lc,Grp,Decs,Bnd),Q);
    if traceNormalize! then
      showMsg("lift let exp @ $(Lc)");
    valis liftLet(Lc,Grp,Decs,Bnd,Map,Q,Free,Ex)
  }
  liftExp(.letRec(Lc,Grp,Decs,Bnd),Map,Q,Ex) => valof{
    Free = findFree(.letRec(Lc,Grp,Decs,Bnd),Q);
    if traceNormalize! then
      showMsg("lift let rec exp $(.letRec(Lc,Grp,Decs,Bnd))");
    valis liftLetRec(Lc,Grp,Decs,Bnd,Map,Q,Free,Ex).
  }
  liftExp(.lambda(Lc,FullNm,Eqn,Tp),Map,Q,Ex) => valof{
    valis liftLambda(.lambda(Lc,FullNm,Eqn,Tp),Map,Q,Ex)
    -- valis liftExp(.letExp(Lc,[.funDef(Lc,FullNm,[Eqn],[],Tp)],
    -- 	[.funDec(Lc,FullNm,FullNm,Tp)],
    -- 	.vr(Lc,FullNm,Tp)),Map,Q,Ex)
  }
  liftExp(.thunk(Lc,Lm,Tp),Map,Q,Ex) => valof{
    if traceNormalize! then
      showMsg("lift thunk $(.thunk(Lc,Lm,Tp))\:$(Tp)");
    valis liftExp(Lm,Map,Q,Ex);
  }
  liftExp(.thRef(Lc,Th,Tp),Map,Q,Ex) => valof{
    if traceNormalize! then
      showMsg("lift thunk ref $(.thRef(Lc,Th,Tp))\:$(Tp)");
    (Thk,Ex1) = liftExp(Th,Map,Q,Ex);
    valis (.cOCall(Lc,Thk,[],Tp),Ex1)
  }
  liftExp(.newSav(Lc,Tp),Map,Q,Ex) => (.cSv(Lc,Tp),Ex).
  liftExp(.svSet(Lc,S,V),Map,Q,Ex) => valof{
    (SS,Ex1) = liftExp(S,Map,Q,Ex);
    (VV,Ex2) = liftExp(V,Map,Q,Ex1);
    valis (.cSvSet(Lc,SS,VV),Ex2)
  }
  liftExp(.cell(Lc,V,Tp),Map,Q,Ex) => valof{
    (VV,Ex1) = liftExp(V,Map,Q,Ex);
    valis (.cCel(Lc,VV,Tp),Ex1)
  }
  liftExp(.get(Lc,V,Tp),Map,Q,Ex) => valof{
    (VV,Ex1) = liftExp(V,Map,Q,Ex);
    valis (.cGet(Lc,VV,Tp),Ex1)
  }
  liftExp(.csexp(Lc,Gov,Cses,Tp),Map,Q,Ex) => valof{
    (LGov,Ex1) = liftExp(Gov,Map,Q,Ex);
    (Cs,Ex2) = transformRules(Cses,Map,Map,Q,.none,Ex1);
    if .cVar(_,_).=LGov then{
      Reslt = caseMatcher(Lc,Map,LGov,.cAbort(Lc,"no matches",Tp),Cs);
      valis (Reslt,Ex2)
    } else {
      V = genVar("C",typeOf(LGov));
      Res = caseMatcher(Lc,Map,.cVar(Lc,V),.cAbort(Lc,"no matches",Tp),Cs);
      valis (.cLtt(Lc,V,LGov,Res),Ex2)
    }
  }
  liftExp(.trycatch(Lc,B,Th,Hndlr,Tp),Map,Q,Ex) => valof{
    if traceNormalize! then
      showMsg("type of $(Th)\:$(typeOf(Th))");
    ErTp = typeOf(Th);
    (BB,Ex2) = liftExp(B,Map,Q,Ex);
    (Hs,Ex3) = transformRules(Hndlr,Map,Map,Q,.none,Ex2);
    ErrVr = .cVar(Lc,genVar("E",ErTp));
    HH = caseMatcher(Lc,Map,ErrVr,.cAbort(Lc,"no matches",Tp),Hs);
    valis (.cTry(Lc,BB,ErrVr,HH,Tp),Ex3)
  }
  liftExp(.thrw(Lc,E,Tp,_),Map,Q,Ex) => valof{
    (LE,Ex2) = liftExp(E,Map,Q,Ex);
    valis (.cThrw(Lc,LE,Tp),Ex)
  }
  liftExp(.resum(Lc,T,M,Tp),Map,Q,Ex) => valof{
    (TT,Ex1) = liftExp(T,Map,Q,Ex);
    (MM,Ex2) = liftExp(M,Map,Q,Ex1);
    valis (.cResum(Lc,TT,MM,Tp),Ex2)
  }
  liftExp(.susp(Lc,T,M,Tp),Map,Q,Ex) => valof{
    (TT,Ex1) = liftExp(T,Map,Q,Ex);
    (MM,Ex2) = liftExp(M,Map,Q,Ex1);
    valis (.cSusp(Lc,TT,MM,Tp),Ex2)
  }
  liftExp(.retyre(Lc,T,M,Tp),Map,Q,Ex) => valof{
    (TT,Ex1) = liftExp(T,Map,Q,Ex);
    (MM,Ex2) = liftExp(M,Map,Q,Ex1);
    valis (.cRetyr(Lc,TT,MM,Tp),Ex2)
  }
  liftExp(.vlof(Lc,A,Tp),Map,Q,Ex) => valof{
    (Acts,Ex1) = liftAction(A,Map,Q,Ex);
    valis (.cValof(Lc,Acts,Tp),Ex1)
  }
  
  liftExps:(cons[canon],set[cV],nameMap,cons[cDefn]) => (cons[cExp],cons[cDefn]).
  liftExps([],_,_,Ex) => ([],Ex).
  liftExps([P,..Ps],Q,Map,Ex) => valof{
    (A,Ex1) = liftExp(P,Map,Q,Ex);
    (As,Exx) = liftExps(Ps,Q,Map,Ex1);
    valis ([A,..As],Exx)
  }

  liftVarExp:(option[locn],string,tipe,nameMap) => cExp.
  liftVarExp(Lc,Nm,Tp,Map) where Entry ?= lookupVarName(Map,Nm) =>
    implementVarExp(Lc,Entry,Map,Tp).
  liftVarExp(Lc,Nm,Tp,Map) => .cVar(Lc,.cV(Nm,Tp)).

  implementVarExp:(option[locn],nameMapEntry,nameMap,tipe) => cExp.
  implementVarExp(Lc,.localFun(_,ClNm,Ar,ThVr),Map,Tp) =>
    .cClos(Lc,ClNm,Ar,liftVarExp(Lc,cName(ThVr),typeOf(ThVr),Map),Tp).
  implementVarExp(Lc,.thunkArg(Base,VFn,_),Map,Tp) => valof{
    V = liftVarExp(Lc,cName(Base),typeOf(Base),Map);
    valis .cCall(Lc,VFn,[V],Tp)
  }
  implementVarExp(Lc,.labelArg(Base,Ix),Map,Tp) => valof{
    V = liftVarExp(Lc,cName(Base),typeOf(Base),Map);
    valis .cNth(Lc,V,Ix,Tp)
  }.
  implementVarExp(Lc,.localVar(Vr),_,Tp) => Vr.
  implementVarExp(Lc,.moduleCons(Enum,CTp),_,Tp) => .cTerm(Lc,Enum,[],Tp).
  implementVarExp(Lc,.localCons(Enum,CTp,Vr),Map,Tp) => valof{
    V = liftVarExp(Lc,cName(Vr),typeOf(Vr),Map);
    valis .cTerm(Lc,Enum,[V],Tp)
  }
  implementVarExp(Lc,.moduleFun(V,_),_,Tp) => V.
  implementVarExp(Lc,.globalVar(Nm,GTp),_,Tp) => .cVar(Lc,.cV(Nm,GTp)).
  implementVarExp(Lc,E,_,Tp) => valof{
    reportError("cannot transform variable $(E)",Lc);
    valis .cVoid(Lc,Tp)
  }

  liftExpCallOp:(option[locn],canon,cons[cExp],tipe,nameMap,set[cV],cons[cDefn]) =>
    crFlow[cExp].
  liftExpCallOp(Lc,.vr(_,Nm,_),Args,Tp,Map,_,Ex) where isEscape(Nm) =>
    (.cCall(Lc,Nm,Args,Tp),Ex).
  liftExpCallOp(Lc,.vr(_,Nm,_),Args,Tp,Map,_,Ex) where Entry ?= lookupVarName(Map,Nm) =>
    implementFunCall(Lc,Entry,Nm,Args,Tp,Map,Ex).
  liftExpCallOp(Lc,.enm(_,Nm,_),Args,Tp,Map,_,Ex) where Entry ?= lookupVarName(Map,Nm) =>
    implementFunCall(Lc,Entry,Nm,Args,Tp,Map,Ex).
  liftExpCallOp(Lc,Op,Args,Tp,Map,Q,Ex) => valof{
    (LOp,Ex0) = liftExp(Op,Map,Q,Ex);
    valis (.cOCall(Lc,LOp,Args,Tp),Ex0)
  }
  liftExpCallOp(Lc,Op,Args,Tp,_,_,_) => valof{
    reportError("cannot compile function $(Op) applied to $(Args)",Lc);
    valis (.cVoid(Lc,Tp),[])
  }.

  implementFunCall:(option[locn],nameMapEntry,string,cons[cExp],tipe,nameMap,cons[cDefn]) =>
    crFlow[cExp].
  implementFunCall(Lc,.moduleFun(_,Fn),_,Args,Tp,Map,Ex) =>
    (.cCall(Lc,Fn,Args,Tp),Ex).
  implementFunCall(Lc,.moduleCons(Fn,FTp),_,Args,Tp,Map,Ex) =>
    (.cTerm(Lc,Fn,Args,Tp),Ex).
  implementFunCall(Lc,.localCons(Fn,FTp,Vr),_,Args,Tp,Map,Ex) => valof{
    VV=liftVarExp(Lc,cName(Vr),typeOf(Vr),Map);
    valis (.cTerm(Lc,Fn,[VV,..Args],Tp),Ex)
  }
  implementFunCall(Lc,.localFun(Nm,_,_,Th),_,Args,Tp,Map,Ex) => valof{
    V = liftVarExp(Lc,cName(Th),typeOf(Th),Map);
    valis (.cCall(Lc,Nm,[V,..Args],Tp),Ex)
  }
  implementFunCall(Lc,.labelArg(Base,Ix),_,Args,Tp,Map,Ex) => valof{
    V = liftVarExp(Lc,cName(Base),typeOf(Base),Map);
    valis (.cOCall(Lc,.cNth(Lc,V,Ix,Tp),Args,Tp),Ex)
  }
  implementFunCall(Lc,.thunkArg(Base,VFn,Ix),_,Args,Tp,Map,Ex) => valof{
    V = liftVarExp(Lc,cName(Base),typeOf(Base),Map);
    valis (.cOCall(Lc,.cCall(Lc,VFn,[V],Tp),Args,Tp),Ex)
  }
  implementFunCall(Lc,.localVar(Vr),_,Args,Tp,Map,Ex) =>
    (.cOCall(Lc,Vr,Args,Tp),Ex).
  implementFunCall(Lc,.globalVar(Nm,GTp),_,Args,Tp,Map,Ex) =>
    (.cOCall(Lc,.cVar(Lc,.cV(Nm,GTp)),Args,Tp),Ex).
  implementFunCall(Lc,V,Vr,Args,Tp,Map,Ex) => valof{
    reportError("illegal variable $(Vr) - $(V)",Lc);
    valis (.cVoid(Lc,Tp),[])
  }

  liftLambda:(canon,nameMap,set[cV],cons[cDefn]) => crFlow[cExp].
  liftLambda(.lambda(Lc,FullNm,Eqn,Tp),Outer,Q,Ex) => valof{
    if traceNormalize! then
      showMsg("lift lambda $(.lambda(Lc,FullNm,Eqn,Tp))\:$(Tp)");

    Free = findFree(.lambda(Lc,FullNm,Eqn,Tp),Q);
    rawFree = freeLabelVars(Free,Outer)::cons[cV];
    varParents = freeParents(rawFree,Outer);
    freeVars = reduceFreeArgs(varParents,Outer);

    ThV = genVar("_ThVr",typeOf(freeVars));
    ThVr = .cVar(Lc,ThV);

    ATp = extendFunTp(deRef(Tp),.some(ThVr));
    
    L = collectLabelVars(freeVars,ThV,0,[]);

    M = [.lyr(.some(ThV),L,[]),..Outer];

    freeArgs = (freeVars//(.cV(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer));
    LamFree = crTpl(Lc,freeArgs);

    ((_,NArgs,NG,NBody),Ex1) = transformRule(Eqn,M,Outer,Q\+ThV,.some(ThVr),Ex);

    Closure = .cClos(Lc,FullNm,arity(ATp),LamFree,Tp);
    LamDefn = .fnDef(Lc,FullNm,ATp,NArgs,(G ?= NG ??
	.cCnd(Lc,G,NBody,.cAbort(Lc,"lambda args failed",Tp)) ||
	NBody));

    if traceNormalize! then
      showMsg("lambda lifted to $(Closure), new defn: $(LamDefn)");

    valis (Closure,[LamDefn,..Ex1])
  }

  liftLet:all e,x ~~ transform[e->>x],letify[x], display[x] |:
    (option[locn],cons[canonDef],cons[decl],e,nameMap,set[cV],set[cV],cons[cDefn]) =>
      crFlow[x].
  liftLet(Lc,Grp,Decls,Bnd,Outer,Q,Free,Ex) => valof{
    (lclVars,glDefs) = unzip(varDefs(Grp));

    lVars = (lclVars//((.cV(Lvn,Ltp))=>.cV(Lvn,savType(Ltp))));
    CM = makeConsMap(Decls);
    GrpFns = (Grp^/(D)=>~_?=isVarDef(D));

    rawGrpFree = freeLabelVars(Free,Outer)::cons[cV];

    ffreeVars = rawGrpFree \ lVars;

    varParents = freeParents(ffreeVars,Outer);
    freeVars = reduceFreeArgs(varParents,Outer);
    
    allFree = freeVars++lVars;

    if traceNormalize! then{
      showMsg("var definitions in let group $(lclVars)");
      showMsg("var definitions in let group $(glDefs)");
      showMsg("freeVars: $(freeVars)");
      showMsg("lVars: $(lVars)");
      showMsg("allFree: $(allFree)");
    };

    if isEmpty(allFree) then{
      MM = pkgMap(Decls,Outer);
      Ex1 = transformGroup(GrpFns,Outer,MM,[],.none,Ex);

      if traceNormalize! then
	showMsg("let functions (0) $(Ex1)");
      valis transform(Bnd,MM,Q,Ex1)
    } else if [SFr] .= allFree && isEmpty(lVars) then {
      MM = [.lyr(.some(SFr),foldRight((D,LL)=>collectMtd(D,.some(SFr),LL),[],Decls),CM),..Outer];
      M = Outer;
      GrpQ = foldLeft(collectQ,Q\+SFr,Grp);
      Ex1 = transformGroup(Grp,Outer,MM,GrpQ,.some(.cVar(Lc,SFr)),Ex);

      if traceNormalize! then{
	showMsg("let functions (1): $(Ex1)");
      };
      
      valis transform(Bnd,MM,GrpQ,Ex1);
    } else {
      ThV = genVar("_ThVr",typeOf(freeVars++lVars));
      ThVr = .cVar(Lc,ThV);

      CM = makeConsMap(Decls);

      L = collectThunkVars(lVars,ThV,size(freeVars),collectLabelVars(freeVars,ThV,0,[]));

      MM = [.lyr(.some(ThV),foldRight((D,LL)=>collectMtd(D,.some(ThV),LL),L,Decls),CM),..Outer];

      M = [.lyr(.some(ThV),L,CM),..Outer];

      freeArgs = (freeVars//(.cV(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer));
      GrpQ = foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q\+ThV,lVars),Grp);

      cellVoids = (glDefs//(E)=>.cVoid(Lc,typeOf(E)));
      GrpFree = crTpl(Lc,freeArgs++cellVoids);

      if traceNormalize! then{
	showMsg("lVars = $(lVars)");
	showMsg("glDefs = $(glDefs)");
	showMsg("GrpFree = $(GrpFree)");
      };

      (Fx,Ex2) = transformLetDefs(Grp,M,MM,GrpQ,.some(ThVr),[],Ex);
      if traceNormalize! then{
	showMsg("fixups $(Fx)");
      };

      (BndTrm,Exx) = transform(Bnd,MM,GrpQ,Ex2);
      valis (computeFixups(Fx,Lc,ThV,GrpFree,BndTrm),Exx)
    }
  }

  liftLetRec:all e,x ~~ transform[e->>x],letify[x] |:
    (option[locn],cons[canonDef],cons[decl],e,nameMap,set[cV],set[cV],
      cons[cDefn]) => crFlow[x].
  liftLetRec(Lc,Grp,Decls,Bnd,Outer,Q,Free,Ex) => valof{
    if traceNormalize! then
      showMsg("let rec at $(Lc)");
    
    (lclVars,glDefs) = unzip(varDefs(Grp));
    lVars = (lclVars//((.cV(Lvn,Ltp))=>.cV(Lvn,savType(Ltp))));

    rawGrpFree = freeLabelVars(Free,Outer)::cons[cV];
    varParents = freeParents(rawGrpFree \ lVars,Outer);
    freeVars = reduceFreeArgs(varParents,Outer);

    if isEmpty(freeVars) && isEmpty(glDefs) then {
      MM = pkgMap(Decls,Outer);
      Ex1 = transformGroup(Grp,MM,MM,[],.none,Ex);
      valis transform(Bnd,MM,Q,Ex1)
    } else if [SFr] .= freeVars && isEmpty(glDefs) then {
      CM = makeConsMap(Decls);

      MM = [.lyr(.some(SFr),foldRight((D,LL)=>collectMtd(D,.some(SFr),LL),[],Decls),CM),..Outer];
      GrpQ = foldLeft(collectQ,Q\+SFr,Grp);
      (Fx,Ex1) = transformLetDefs(Grp,MM,MM,GrpQ,.some(.cVar(Lc,SFr)),[],Ex);
      if traceNormalize! then{
	showMsg("fixups $(Fx)");
      };
      if ~isEmpty(Fx) then
	reportError("expecting fixups to be empty, not $(Fx)",Lc);

      valis transform(Bnd,MM,GrpQ,Ex1)
    } else{
      ThV = genVar("_ThVr",typeOf(freeVars++lVars));
      ThVr = .cVar(Lc,ThV);

      CM = makeConsMap(Decls);

      L = collectThunkVars(lVars,ThV,size(freeVars),collectLabelVars(freeVars,ThV,0,[]));
      
      M = [.lyr(.some(ThV),foldRight((D,LL)=>collectMtd(D,.some(ThV),LL),L,Decls),CM),..Outer];

      freeArgs = (freeVars//(.cV(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer));
      GrpQ = foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q\+ThV,lVars),Grp);

      cellVoids = (glDefs//(E)=>.cVoid(Lc,typeOf(E)));
      GrpFree = crTpl(Lc,freeArgs++cellVoids);

      if traceNormalize! then{
	showMsg("lVars = $(lVars)");
	showMsg("glDefs = $(glDefs)");
	showMsg("GrpFree = $(GrpFree)");
      };

      if traceNormalize! then{
	showMsg("free term = $(GrpFree)")
      };
      (Fx,Ex2) = transformLetDefs(Grp,M,M,GrpQ,.some(ThVr),[],Ex);
      if traceNormalize! then{
	showMsg("fixups $(Fx)");
      };

      (BndTrm,Exx) = transform(Bnd,M,GrpQ,Ex2);
      valis (computeFixups(Fx,Lc,ThV,GrpFree,BndTrm),Exx)
    }
  }

  liftFreeThunk(Lc,Nm,Val,Tp,Ix,ThVr,Outer,Q,Fx,Ex) => valof{
    if traceNormalize! then
      showMsg("lift $(Nm)\:$(Tp) = $(Val) @ $(Lc)");

    FrTp = typeOf(ThVr);
    (VV,Ex1) = liftExp(Val,Outer,Q,Ex);
    SV = .cVar(Lc,genVar("_SVr",savType(Tp)));
    X = .cVar(Lc,genVar("ϕ",Tp));
    TV = .cVar(Lc,ThVr);

    ThDf = .fnDef(Lc,Nm,funType([typeOf(ThVr)],Tp),
      [TV],
      .cValof(Lc,
	.aSeq(Lc,
	  .aDefn(Lc,SV,.cNth(Lc,TV,Ix,savType(Tp))),
	  .aIftte(Lc,.cMatch(Lc,.cSvDrf(Lc,X,Tp),SV),
	    .aValis(Lc,X),
	    .aValis(Lc,.cSvSet(Lc,SV,VV)))),Tp));
    valis ([(Nm,Ix,.cSv(Lc,savType(Tp))),..Fx],[ThDf,..Ex1])
  }

  fixUp ~> (string,integer,cExp).

  computeFixups:all e ~~ letify[e] |: (cons[fixUp],option[locn],cV,cExp,e) => e.
  computeFixups([],Lc,Vr,Fr,Bnd) => letify(Lc,Vr,Fr,Bnd).
  computeFixups([(Nm,Ix,Up),..Fx],Lc,Vr,Fr,Bnd) => valof{
    if traceNormalize! then{
      showMsg("compute fixup for $(Nm) at $(Ix) = $(Up)");
      showMsg("present: $(Vr) in $(Up) $(present(Up,(T) => (.cVar(_,VV).=T ?? VV==Vr || .false)))");
    };
    
    if ~present(Up,(T) => (.cVar(_,VV).=T ?? VV==Vr || .false)) &&
	.cTerm(FLc,FOp,FArgs,FTp) .= Fr then{
	  Fr1 = .cTerm(FLc,FOp,FArgs[Ix->Up],FTp);
	  if traceNormalize! then
	    showMsg("fixed up free term $(Fr1)");
	  valis computeFixups(Fx,Lc,Vr,Fr1,Bnd)
	}
    else{
      valis computeFixups(Fx,Lc,Vr,Fr,freeUpdate(Lc,.cVar(Lc,Vr),Ix,Up,Bnd))
    }
  }

  transformLetDefs:(cons[canonDef],nameMap,nameMap,set[cV],option[cExp],cons[fixUp],cons[cDefn]) =>
    (cons[fixUp],cons[cDefn]).
  transformLetDefs([],_,_,_,_,Fx,D) => (Fx,D).
  transformLetDefs([D,..Ds],Map,Outer,Q,Extra,Fx,Ex) => valof {
    (Fx1,Ex1) = transformLetDef(D,Map,Outer,Q,Extra,Fx,Ex);
    valis transformLetDefs(Ds,Map,Outer,Q,Extra,Fx1,Ex1)
  }

  transformLetDef:(canonDef,nameMap,nameMap,set[cV],option[cExp],cons[fixUp],cons[cDefn]) => (cons[fixUp],cons[cDefn]).
  transformLetDef(.funDef(Lc,FullNm,Eqns,_,Tp),Map,Outer,Q,Extra,Fx,Ex) => valof{
    Ex1 = transformFunction(Lc,FullNm,Eqns,Tp,Map,Outer,Q,Extra,Ex);
    valis (Fx,Ex1)
  }
  transformLetDef(.varDef(Lc,_,FullNm,.lambda(_,_,Eqn,Tp),_,_),Map,Outer,Q,Extra,Fx,Ex) => valof{
    Ex1 = transformFunction(Lc,FullNm,[Eqn],Tp,Map,Outer,Q,Extra,Ex);
    valis (Fx,Ex1)
  }
  transformLetDef(.varDef(Lc,_,FullNm,Val,Cx,Tp),Map,Outer,Q,.none,Fx,Ex) => valof{
    (Vl,Defs) = liftExp(Val,Outer,Q,Ex);
    valis (Fx,[.glDef(Lc,FullNm,Tp,Vl),..Defs])
  }
  transformLetDef(.varDef(Lc,Nm,FullNm,Val,Cx,Tp),Map,Outer,Q,.some(V),Fx,Ex) where .cVar(VLc,ThVr) .= V => valof{
    if (_,Ix) ?= thunkIndex(FullNm,Map) then{
      valis liftFreeThunk(Lc,FullNm,Val,Tp,Ix,ThVr,Outer,Q,Fx,Ex)
    } else
    reportError("(internal) expecting correct thunk index for $(Nm)",Lc);
    valis (Fx,Ex)
  }
  transformLetDef(.implDef(Lc,Nm,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Fx,Ex) =>
    transformLetDef(.varDef(Lc,Nm,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Fx,Ex).
  transformLetDef(.typeDef(Lc,Nm,Tp,TpRl),Map,_,_,_,Fx,Ex) => valof{
    Ex1 = transformTypeDef(Lc,Nm,Tp,TpRl,Map,Ex);
    valis (Fx,Ex1)
  }
  transformLetDef(.cnsDef(Lc,Nm,Ix,Tp),Map,_,_,_,Fx,Ex) => valof{
    Ex1 = transformConsDef(Lc,Nm,Ix,Tp,Map,Ex);
    valis (Fx,Ex1)
  }

  liftAction:(canonAction,nameMap,set[cV],cons[cDefn]) => (aAction,cons[cDefn]).
  liftAction(.doNop(Lc),_,_,Ex) => (.aNop(Lc),Ex).
  liftAction(.doSeq(Lc,L,R),Map,Q,Ex) => valof{
    (LL,Ex1) = liftAction(L,Map,Q,Ex);
    (RR,Ex2) = liftAction(R,Map,actnVars(L,Q),Ex1);
    valis (.aSeq(Lc,LL,RR),Ex2)
  }
  liftAction(.doLbld(Lc,Lb,A),Map,Q,Ex) => valof{
    (AA,Ex1) = liftAction(A,Map,Q,Ex);
    valis (.aLbld(Lc,Lb,AA),Ex1)
  }
  liftAction(.doBrk(Lc,Lb),_,_,Ex) => (.aBreak(Lc,Lb),Ex).
  liftAction(.doValis(Lc,E),Map,Q,Ex) => valof{
    (EE,Ex1) = liftExp(E,Map,Q,Ex);
    valis (.aValis(Lc,EE),Ex1)
  }
  liftAction(.doDefn(Lc,P,E),Map,Q,Ex) => valof{
    (PP,Ex1) = liftPtn(P,Map,Q,Ex);
    (EE,Ex2) = liftExp(E,Map,Q,Ex1);
    valis (.aDefn(Lc,PP,EE),Ex2)
  }
  liftAction(.doMatch(Lc,P,E),Map,Q,Ex) => valof{
    (PP,Ex1) = liftPtn(P,Map,Q,Ex);
    (EE,Ex2) = liftExp(E,Map,Q,Ex1);
    valis (.aMatch(Lc,PP,EE),Ex2)
  }
  liftAction(.doAssign(Lc,P,E),Map,Q,Ex) => valof{
    (PP,Ex1) = liftExp(P,Map,Q,Ex);
    (EE,Ex2) = liftExp(E,Map,Q,Ex1);
    valis (.aAsgn(Lc,PP,EE),Ex2)
  }
  liftAction(.doIfThen(Lc,C,L,R),Map,Q,Ex) => valof{
    (CC,Ex1) = liftExp(C,Map,Q,Ex);
    (LL,Ex2) = liftAction(L,Map,condVars(C,Q),Ex1);
    (RR,Ex3) = liftAction(R,Map,Q,Ex2);
    valis (.aIftte(Lc,CC,LL,RR),Ex3)
  }
  liftAction(.doWhile(Lc,C,B),Map,Q,Ex) => valof{
    (CC,Ex1) = liftExp(C,Map,Q,Ex);
    (BB,Ex2) = liftAction(B,Map,condVars(C,Q),Ex1);
    valis (.aWhile(Lc,CC,BB),Ex2)
  }
  liftAction(.doCase(Lc,Gv,Cs),Map,Q,Ex) => valof{
    (LGv,Ex1) = liftExp(Gv,Map,Q,Ex);
    (CCs,Ex2) = transformRules(Cs,Map,Map,Q,.none,Ex1);
    if .cVar(_,_).=LGv then{
      Reslt = caseMatcher(Lc,Map,LGv,.aAbort(Lc,"no matches"),CCs);

      valis (Reslt,Ex2)
    } else {
      V = genVar("C",typeOf(Gv));
      Res = caseMatcher(Lc,Map,.cVar(Lc,V),.aAbort(Lc,"no matches"),CCs);
      valis (.aLtt(Lc,V,LGv,Res),Ex2)
    }
  }
  liftAction(.doTry(Lc,B,ErTp,H),Map,Q,Ex) => valof{
    (BB,Ex2) = liftAction(B,Map,Q,Ex);
    (Hs,Ex3) = transformRules(H,Map,Map,Q,.none,Ex2);
    ErrVr = .cVar(Lc,genVar("E",ErTp));
    Hndlr = caseMatcher(Lc,Map,ErrVr,.aAbort(Lc,"no matches"),Hs);
    
    valis (.aTry(Lc,BB,ErrVr,Hndlr),Ex3)
  }
  liftAction(.doThrow(Lc,E),Map,Q,Ex) => valof{
    (EE,Ex1) = liftExp(E,Map,Q,Ex);
    valis (.aThrw(Lc,EE),Ex1)
  }
  liftAction(.doExp(Lc,E),Map,Q,Ex) => valof{
    (EE,Ex1) = liftExp(E,Map,Q,Ex);
    valis (.aDo(Lc,EE),Ex1)
  }
  liftAction(.doLet(Lc,Grp,Dcs,Bnd),Map,Q,Ex) => valof{
    Free = findFree(.doLet(Lc,Grp,Dcs,Bnd),Q);

    if traceNormalize! then
      showMsg("lift let action $(.doLet(Lc,Grp,Dcs,Bnd))");
    
    valis liftLet(Lc,Grp,Dcs,Bnd,Map,Q,Free,Ex)
  }
  liftAction(.doLetRec(Lc,Grp,Dcs,Bnd),Map,Q,Ex) => valof{
    Free = findFree(.doLetRec(Lc,Grp,Dcs,Bnd),Q);
    valis liftLetRec(Lc,Grp,Dcs,Bnd,Map,Q,Free,Ex)
  }
  
  varDefs:(cons[canonDef]) => cons[(cV,canon)].
  varDefs(Defs) =>
    foldLeft((D,FF) => (V?=isVarDef(D) ?? [V,..FF] || FF),
      [],Defs).

  isVarDef(.varDef(_,_,FullNm,Vl,_,Tp)) where ~isFunDef(Vl) =>
    .some((.cV(FullNm,Tp),Vl)).
  isVarDef(.implDef(_,_,Nm,Vl,_,Tp)) where ~isFunDef(Vl) =>
    .some((.cV(Nm,Tp),Vl)).
  isVarDef(_) default => .none.

  collectLabelVars:(cons[cV],cV,integer,map[string,nameMapEntry]) =>
    map[string,nameMapEntry].
  collectLabelVars([],_,_,LV) => LV.
  collectLabelVars([.cV(Nm,Tp),..Vrs],ThV,Ix,Entries) =>
    collectLabelVars(Vrs,ThV,Ix+1,Entries[Nm->.labelArg(ThV,Ix)]).
  
  collectThunkVars:(cons[cV],cV,integer,map[string,nameMapEntry]) =>
    map[string,nameMapEntry].
  collectThunkVars([],_,_,LV) => LV.
  collectThunkVars([.cV(Nm,Tp),..Vrs],ThV,Ix,Entries) =>
    collectThunkVars(Vrs,ThV,Ix+1,Entries[Nm->.thunkArg(ThV,Nm,Ix)]).
  
  -- eliminate free variables that can be computed from other free vars
  reduceFreeArgs:(cons[cV],nameMap) => cons[cV].
  reduceFreeArgs(FrVrs,Map) => let{.
    reduceArgs:(cons[cV],cons[cV]) => cons[cV].
    reduceArgs([],Frs) => Frs.
    reduceArgs([FrV,..FrArgs],Frs) where
	OTh ?= lookupThetaVar(Map,cName(FrV)) &&
	OTh .<. Frs =>
      reduceArgs(FrArgs,drop(FrV,Frs)).
    reduceArgs([_,..FrArgs],Frs) => reduceArgs(FrArgs,Frs).
  .} in reduceArgs(FrVrs,FrVrs).

  freeParents:(cons[cV],nameMap) => cons[cV].
  freeParents(Frs,Map) => foldLeft((F,Fs)=>Fs\+freeParent(F,Map),[],Frs).

  freeParent(V,Map) where ThV ?= lookupThetaVar(Map,cName(V)) =>
    freeParent(ThV,Map).
  freeParent(V,_) default => V.

  collectMtd:(decl,option[cV],map[string,nameMapEntry])=>map[string,nameMapEntry].
  collectMtd(.funDec(Lc,Nm,FullNm,Tp),.some(ThVr),LL) => valof{
    Entry = .localFun(FullNm,closureNm(FullNm),arity(Tp)+1,ThVr);
    valis LL[Nm->Entry][FullNm->Entry]
  }
  collectMtd(.funDec(Lc,Nm,FullNm,Tp),.none,LL) => valof{
    Entry = .moduleFun(.cClos(Lc,closureNm(FullNm),arity(Tp)+1,crTpl(Lc,[]),Tp),Nm);
    valis LL[Nm->Entry][FullNm->Entry]
  }
  collectMtd(.varDec(Lc,Nm,Val,Tp),.none,LL) => LL[Nm->.globalVar(Nm,Tp)].
  collectMtd(.varDec(Lc,Nm,Val,Tp),.some(ThVr),LL) => LL.
  collectMtd(.cnsDec(Lc,Nm,FullNm,Tp),.none,LL) => LL[Nm->.moduleCons(FullNm,Tp)].
  collectMtd(.cnsDec(Lc,Nm,FullNm,Tp),.some(ThVr),LL) => LL[Nm->.localCons(FullNm,Tp,ThVr)].
  collectMtd(_,_,LL) default => LL.

  collectQ:(canonDef,set[cV]) => set[cV].
  collectQ(.funDef(_,Nm,_,_,Tp),Q) => Q\+.cV(Nm,Tp).
  collectQ(.varDef(_,_,Nm,Val,_,Tp),Q) => Q\+.cV(Nm,Tp).
  collectQ(.implDef(_,_,FullNm,Val,_,Tp),Q) => Q\+.cV(FullNm,Tp).
  collectQ(.typeDef(_,_,_,_),Q) => Q.
  collectQ(.cnsDef(_,_,_,_),Q) => Q.

  freeLabelVars:(set[cV],nameMap)=>set[cV].
  freeLabelVars(Fr,Map) => foldLeft((V,So)=>labelVar(V,Map,So),Fr,Fr).

  labelVar:(cV,nameMap,set[cV])=>set[cV].
  labelVar(.cV(Nm,_),Map,So) where Entry?=lookupVarName(Map,Nm) =>
    case Entry in {
    | .labelArg(ThVr,_) => So\+ThVr
    | .thunkArg(ThVr,_,_) => So\+ThVr
    | .localFun(_,_,_,ThVr) => So\+ThVr
    | _ => So
    }.
  labelVar(_,_,So) default => So.

  thunkIndex:(string,nameMap) => option[(cV,integer)].
  thunkIndex(Nm,Map) => valof{
    if E?=lookupVarName(Map,Nm) then{
      case E in {
	| .thunkArg(Thv,_,Ix) => valis .some((Thv,Ix))
	| _ default => valis .none
      }
    } else
    valis .none
  }

  makeFunVars:(tipe)=>cons[cV].
  makeFunVars(Tp) where .tupleType(Es)?=funTypeArg(deRef(Tp)) => (Es//(E)=>genVar("_",E)).
}
