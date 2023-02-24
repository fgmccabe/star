star.compiler.normalize{
  import star.
  import star.pkg.
  import star.sort.

  import star.compiler.canon.
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
    Map = pkgMap(Decls);
    valis transformGroup(Defs,Map,Map,[],.none,[])
  }

  transformGroup:(cons[canonDef],nameMap,nameMap,set[cId],option[cExp],cons[cDefn]) =>
    cons[cDefn].
  transformGroup([],_,_,_,_,D) => D.
  transformGroup([D,..Ds],Map,Outer,Q,Extra,Ex) => valof {
    Ex1 = transformDef(D,Map,Outer,Q,Extra,Ex);
    valis transformGroup(Ds,Map,Outer,Q,Extra,Ex1)
  }

  all e ~~ crFlow[e] ~> (e,cons[cDefn]).

  transformDef:(canonDef,nameMap,nameMap,set[cId],option[cExp],cons[cDefn]) =>cons[cDefn].
  transformDef(.varDef(Lc,FullNm,.lambda(_,LNm,Eqns,Tp),_,_),Map,Outer,Q,Extra,Ex) =>
    transformFunction(Lc,FullNm,Eqns,Tp,Map,Outer,Q,Extra,Ex).
  transformDef(.varDef(Lc,FullNm,Val,Cx,Tp),Map,Outer,Q,.none,Ex) => valof{
    (Vl,Defs) = liftExp(Val,Outer,Q,Ex);
    valis [.vrDef(Lc,FullNm,Tp,Vl),..Defs]
  }
  transformDef(.varDef(Lc,FullNm,Val,Cx,Tp),Map,Outer,Q,.some(V),Ex) where .cVar(VLc,ThVr) .= V => valof{
    if traceNormalize! then
      logMsg("lift thunk $(FullNm)\:$(Tp) = $(Val) @ $(Lc)");

    ATp = extendFunTp(deRef(Tp),?V);
    if traceNormalize! then
      logMsg("extended function function type $(ATp)");

    Thnk = genVar("Thnk",thunkType(Tp));

    (Vl,Defs) = liftExp(Val,Outer,Q,Ex);
    Body = .cThSet(Lc,.cVar(Lc,Thnk),Vl,Tp);
    if traceNormalize! then
      logMsg("definition body = $(Body)");

    ThunkNm = varClosureNm(FullNm);
    Thunk = .fnDef(Lc,ThunkNm,funType([typeOf(ThVr),thunkType(Tp)],Tp),[ThVr,Thnk],Body);

    valis [Thunk,..Defs]
  }

  transformDef(.implDef(Lc,_,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Ex) =>
    transformDef(.varDef(Lc,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Ex).
  transformDef(.typeDef(Lc,Nm,Tp,TpRl),Map,_,_,_,Ex) =>
    transformTypeDef(Lc,Nm,Tp,TpRl,Map,Ex).
  transformDef(.cnsDef(Lc,Nm,FullNm,Tp),Map,_,_,_,Ex) =>
    transformConsDef(Lc,FullNm,Tp,Map,Ex).
  transformDef(.conDef(_,_,_,_),_,_,_,_,Ex) => Ex.
  transformDef(.accDef(_,_,_,_),_,_,_,_,Ex) => Ex.
  transformDef(.updDef(_,_,_,_),_,_,_,_,Ex) => Ex.

  transformFunction(Lc,FullNm,Eqns,Tp,Map,Outer,Q,Extra,Ex) => valof{
    if traceNormalize! then{
      logMsg("transform function $(.lambda(Lc,FullNm,Eqns,Tp)) Q=$(Q) Extra = $(Extra) @ $(Lc)");
    };
    ATp = extendFunTp(deRef(Tp),Extra);
    if traceNormalize! then
      logMsg("extended function function type $(ATp)");
    
    (Eqs,Ex1) = transformRules(Eqns,Outer,Q,Extra,Ex);
    if traceNormalize! then
      logMsg("transformed equations: $(Eqs)");
    Func = functionMatcher(Lc,FullNm,ATp,Map,Eqs);
    if traceNormalize! then
      logMsg("transformed function $(Func)");

    ClosureNm = closureNm(FullNm);
    ClVar = (.cVar(_,Exv)?=Extra ?? Exv || .cId("_",unitTp));
    ClVars = makeFunVars(Tp);
    ClArgs = [ClVar,..ClVars];

    ClosTp = extendFunTp(deRef(Tp),.some(ClVar));

    if Exv?=Extra then {
      ClosEntry =
	.fnDef(Lc,ClosureNm,ClosTp,ClArgs,
	  .cCall(Lc,FullNm,ClArgs//(V)=>.cVar(Lc,V),funTypeRes(Tp)));
      valis [Func,ClosEntry,..Ex1]
    } else {
      ClosEntry =
	.fnDef(Lc,ClosureNm,ClosTp,
	  ClArgs,.cCall(Lc,FullNm,ClVars//(V)=>.cVar(Lc,V),funTypeRes(Tp)));
      valis [Func,ClosEntry,..Ex1]
    }
  }

  transformTypeDef(Lc,Nm,Tp,TpRl,Map,Ex) => valof{
    if ConsMap ?= findIndexMap(Nm,Map) then
      valis [.tpDef(Lc,Tp,TpRl,ConsMap),..Ex]
    else
    valis [.tpDef(Lc,Tp,TpRl,[]),..Ex]
  }

  transformConsDef(Lc,Nm,Tp,Map,Ex) => valof{
    (_,CT) = deQuant(Tp);
    (_,IT) = deConstrain(CT);
    (ATp,RTp) = ^ isConsType(IT);
    if (Ar,_) ?= isTupleType(ATp) then{
      if ConsMap ?= findIndexMap(tpName(RTp),Map) then{
	(Lbl,_,Ix) = ^ findLbl(Nm,ConsMap);
	valis [.lblDef(Lc,Lbl,Tp,Ix),..Ex]
      }
    };
    valis Ex
  }

  findLbl(Nm,[]) => .none.
  findLbl(Nm,[(.tLbl(Nm,Ar),Tp,Ix),.._]) => .some((.tLbl(Nm,Ar),Tp,Ix)).
  findLbl(Nm,[_,..Ms]) => findLbl(Nm,Ms).

  contract all e,t ~~ transform[e->>t] ::= {
    transform:(e,nameMap,set[cId],cons[cDefn]) => (t,cons[cDefn]).
  }

  implementation transform[canon->>cExp] => {
    transform(E,Map,Q,Ex) => liftExp(E,Map,Q,Ex).
  }

  implementation transform[canonAction->>aAction] => {
    transform(A,Map,Q,Ex) => liftAction(A,Map,Q,Ex).
  }

  contract all t ~~ letify[t] ::= {
    letify:(option[locn],cId,cExp,t) => t.
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

  transformRules:all e,t ~~ transform[e->>t] |:
    (cons[rule[e]],nameMap,set[cId],option[cExp],cons[cDefn]) =>
      (cons[(option[locn],cons[cExp],option[cExp],t)],cons[cDefn]).
  transformRules([],_,_,_,Ex) => ([],Ex).
  transformRules([Eqn,..Eqns],Map,Q,Extra,Ex) => valof{
    (Trple,Ex1) = transformRule(Eqn,Map,Q,Extra,Ex);
    (Rest,Exx) = transformRules(Eqns,Map,Q,Extra,Ex1);
    valis ([Trple,..Rest],Exx)
  }

  transformRule:all e,t ~~ transform[e->>t] |:
    (rule[e],nameMap,set[cId],option[cExp],cons[cDefn]) =>
      ((option[locn],cons[cExp],option[cExp],t),cons[cDefn]).
  transformRule(.rule(Lc,Arg,Test,Val),Map,Q,Extra,Ex) => valof{
    EQ = ptnVars(Arg,Q,[]);
    if traceNormalize! then
      logMsg("Pattern vars $(EQ)");
    (APtn,Ex1) = liftPtn(Arg,Map,EQ,Ex);

    if traceNormalize! then
      logMsg("lifted pattern $(APtn)");

    (TPtn, WC) = pullWhere(APtn);

    GEQ = (Tst?=Test ?? condVars(Tst,EQ) || EQ);
    (NG,Ex2) = liftGoal(Test,Map,GEQ,Ex1);
    (Rep,Exx) = transform(Val,Map,GEQ,Ex2);
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

  liftPtn:(canon,nameMap,set[cId],cons[cDefn]) => crFlow[cExp].
  liftPtn(.anon(Lc,Tp),Map,_,Ex) => (.cAnon(Lc,Tp),Ex).
  liftPtn(.vr(Lc,Nm,Tp),Map,Q,Ex) => trVarPtn(Lc,Nm,Tp,Map,Q,Ex).
  liftPtn(.enm(Lc,FullNm,Tp),Map,_,Ex) => (.cTerm(Lc,FullNm,[],Tp),Ex).
  liftPtn(.intr(Lc,Ix),Map,_,Ex) =>  (.cInt(Lc,Ix),Ex).
  liftPtn(.bintr(Lc,Ix),Map,_,Ex) => (.cBig(Lc,Ix),Ex).
  liftPtn(.flt(Lc,Dx),Map,_,Ex) => (.cFloat(Lc,Dx),Ex).
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
  liftPtn(.apply(Lc,.enm(VLc,FullNm,_),Els,Tp),Map,Q,Ex) => valof{
    (LArgs,Ex1) = liftPtns(Els,Map,Q,Ex);

    valis (.cTerm(Lc,FullNm,LArgs,Tp),Ex1)
  }
  liftPtn(Cn,_,_,Ex) => valof{
    Lc = locOf(Cn);
    reportError("may not have $(Cn) as a pattern",Lc);
    valis (.cVoid(Lc,typeOf(Cn)),Ex)
  }
  
  liftPtns:(cons[canon],nameMap,set[cId],cons[cDefn]) => (cons[cExp],cons[cDefn]).
  liftPtns([],_,_,Ex) => ([],Ex).
  liftPtns([P,..Ps],Map,Q,Ex) => valof{
    (A,Ex1) = liftPtn(P,Map,Q,Ex);
    (As,Exx) = liftPtns(Ps,Map,Q,Ex1);
    valis ([A,..As],Exx)
  }

  liftPtnCallOp:(option[locn],string,cons[cExp],tipe,nameMap,set[cId],cons[cDefn]) =>
    (cExp,cons[cDefn]).
  liftPtnCallOp(Lc,Nm,Args,Tp,Map,Q,Ex) where Entry?= lookupVarName(Map,Nm) =>
    implementPtnCall(Lc,Entry,Args,Tp,Map,Q,Ex).

  implementPtnCall(Lc,.moduleCons(Nm,CTp),Args,Tp,_,_,Ex) =>
    (.cTerm(Lc,Nm,Args,Tp),Ex).
  implementPtnCall(Lc,.localCons(Nm,CTp,Vr),Args,Tp,_,_,Ex) =>
    (.cTerm(Lc,Nm,[.cVar(Lc,Vr),..Args],Tp),Ex).
  
  trVarPtn(Lc,Nm,Tp,Map,Q,Ex) => ({? .cId(Nm,Tp) in Q ?} ??
    (.cVar(Lc,.cId(Nm,Tp)),Ex) ||
    implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex)).

  implementVarPtn(Lc,Nm,.none,Tp,_,Ex) => (.cVar(Lc,.cId(Nm,Tp)),Ex).
  implementVarPtn(Lc,Nm,.some(.moduleCons(Enum,CTp)),Tp,_,Ex) where ETp?=isEnumType(CTp) =>
    (.cTerm(Lc,Enum,[],ETp),Ex).
  implementVarPtn(Lc,Nm,.some(.localCons(Enum,CTp,Vr)),Tp,_,Ex) =>
    (.cTerm(Lc,Enum,[.cVar(Lc,Vr)],Tp),Ex).
  implementVarPtn(Lc,Nm,.some(V),Tp,_Map,Ex) => valof{
    reportError("not permitted to match against $(Nm)\:$(V)",Lc);
    valis (.cVoid(Lc,Tp),Ex)
  }.

  liftExp:(canon,nameMap,set[cId],cons[cDefn]) => crFlow[cExp].
  liftExp(.anon(Lc,Tp),Map,Q,Ex) => (.cAnon(Lc,Tp),Ex).
  liftExp(.vr(Lc,Nm,Tp),Map,Q,Ex) => valof{
    VV = liftVarExp(Lc,Nm,Tp,Map);
    valis (VV,Ex)
  }
  liftExp(.intr(Lc,Ix),_,Map,Ex) => (.cInt(Lc,Ix),Ex).
  liftExp(.bintr(Lc,Ix),_,Map,Ex) => (.cBig(Lc,Ix),Ex).
  liftExp(.flt(Lc,Dx),_,Map,Ex) => (.cFloat(Lc,Dx),Ex).
  liftExp(.kar(Lc,Cx),_,Map,Ex) => (.cChar(Lc,Cx),Ex).
  liftExp(.strng(Lc,Sx),_,Map,Ex) => (.cString(Lc,Sx),Ex).
  liftExp(.enm(Lc,FullNm,Tp),_,Map,Ex) => (.cTerm(Lc,FullNm,[],Tp),Ex).
  liftExp(.tple(Lc,Els),Q,Map,Ex) => valof{
    (LEls,Exx) = liftExps(Els,Map,Q,Ex);
    valis (crTpl(Lc,LEls),Exx)
  }
  liftExp(.apply(Lc,Op,Els,Tp),Q,Map,Ex) => valof{
    (LEls,Ex1) = liftExps(Els,Map,Q,Ex);
    valis liftExpCallOp(Lc,Op,LEls,Tp,Q,Map,Ex1)
  }
  liftExp(.dot(Lc,Rc,Fld,Tp),Map,Q,Ex) => valof{
    reportError("unexpected dot expression $(.dot(Lc,Rc,Fld,Tp))",Lc);
    valis (.cVoid(Lc,Tp),[])
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

--    logMsg("lift wheres from $(LT) = $(liftWhere(LT))");

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
      logMsg("lift let exp $(.letExp(Lc,Grp,Decs,Bnd)), Q=$(Q), Free=$(Free)");
    valis liftLet(Lc,Grp,Decs,Bnd,Map,Q,Free,Ex)
  }
  liftExp(.letRec(Lc,Grp,Decs,Bnd),Map,Q,Ex) => valof{
    Free = findFree(.letRec(Lc,Grp,Decs,Bnd),Q);
    if traceNormalize! then
      logMsg("lift let exp $(.letRec(Lc,Grp,Decs,Bnd))");
    valis liftLetRec(Lc,Grp,Decs,Bnd,Map,Q,Free,Ex).
  }
  liftExp(.lambda(Lc,FullNm,Eqns,Tp),Map,Q,Ex) => 
    liftExp(.letExp(Lc,[.varDef(Lc,FullNm,.lambda(Lc,FullNm,Eqns,Tp),[],Tp)],
	[.funDec(Lc,FullNm,FullNm,Tp)],
	.vr(Lc,FullNm,Tp)),Map,Q,Ex).
  liftExp(.csexp(Lc,Gov,Cses,Tp),Map,Q,Ex) => valof{
    (LGov,Ex1) = liftExp(Gov,Map,Q,Ex);
    (Cs,Ex2) = transformRules(Cses,Map,Q,.none,Ex1);
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
    if ErTp?=getTypeArg(typeOf(Th),"star.core*cont") then{
      (TT,Ex1) = liftPtn(Th,Map,Q,Ex);
      (BB,Ex2) = liftExp(B,Map,ptnVars(Th,Q,[]),Ex1);
      (Hs,Ex3) = transformRules(Hndlr,Map,Q,.none,Ex2);
      ErrVr = .cVar(Lc,genVar("E",ErTp));
      HH = caseMatcher(Lc,Map,ErrVr,.cAbort(Lc,"no matches",Tp),Hs);
      valis (.cTry(Lc,BB,TT,ErrVr,HH,Tp),Ex3)
    }
    else {
      reportError("expecting a continuation type, not $(typeOf(Th))",Lc);
      valis (.cVoid(Lc,typeOf(Th)),Ex)
    }
  }
  liftExp(.rais(Lc,T,E,Tp),Map,Q,Ex) => valof{
    (LT,Ex1) = liftExp(T,Map,Q,Ex);
    (LE,Ex2) = liftExp(E,Map,Q,Ex1);
    valis (.cRaise(Lc,LT,LE,Tp),Ex1)
  }
  liftExp(.vlof(Lc,A,Tp),Map,Q,Ex) => valof{
    (Acts,Ex1) = liftAction(A,Map,Q,Ex);
    valis (.cValof(Lc,Acts,Tp),Ex1)
  }
  
  liftExps:(cons[canon],set[cId],nameMap,cons[cDefn]) => (cons[cExp],cons[cDefn]).
  liftExps([],_,_,Ex) => ([],Ex).
  liftExps([P,..Ps],Q,Map,Ex) => valof{
    (A,Ex1) = liftExp(P,Map,Q,Ex);
    (As,Exx) = liftExps(Ps,Q,Map,Ex1);
    valis ([A,..As],Exx)
  }

  liftVarExp:(option[locn],string,tipe,nameMap) => cExp.
  liftVarExp(Lc,Nm,Tp,Map) where Entry ?= lookupVarName(Map,Nm) =>
    implementVarExp(Lc,Entry,Map,Tp).
  liftVarExp(Lc,Nm,Tp,Map) => .cVar(Lc,.cId(Nm,Tp)).

  implementVarExp:(option[locn],nameMapEntry,nameMap,tipe) => cExp.
  implementVarExp(Lc,.localFun(_,ClNm,ThVr),Map,Tp) => valof{
    V = liftVarExp(Lc,cName(ThVr),typeOf(ThVr),Map);
    valis .cTerm(Lc,ClNm,[V],Tp)
  }
  implementVarExp(Lc,.labelArg(Base,Ix),Map,Tp) => valof{
    V = liftVarExp(Lc,cName(Base),typeOf(Base),Map);
    valis .cNth(Lc,V,Ix,Tp)
  }.
  implementVarExp(Lc,.memoArg(ClNm,Base,Ix,_),Map,Tp) => valof{
    V = liftVarExp(Lc,cName(Base),typeOf(Base),Map);
    valis .cThGet(Lc,.cNth(Lc,V,Ix,Tp),Tp)
  }
  implementVarExp(Lc,.localVar(Vr),_,Tp) => Vr.
  implementVarExp(Lc,.moduleCons(Enum,CTp),_,Tp) => .cTerm(Lc,Enum,[],Tp).
  implementVarExp(Lc,.localCons(Enum,CTp,Vr),Map,Tp) => valof{
    V = liftVarExp(Lc,cName(Vr),typeOf(Vr),Map);
    valis .cTerm(Lc,Enum,[V],Tp)
  }
  implementVarExp(Lc,.moduleFun(V,_),_,Tp) => V.
  implementVarExp(Lc,.globalVar(Nm,GTp),_,Tp) => .cVar(Lc,.cId(Nm,GTp)).
  implementVarExp(Lc,E,_,Tp) => valof{
    reportError("cannot transform variable $(E)",Lc);
    valis .cVoid(Lc,Tp)
  }

  liftExpCallOp:(option[locn],canon,cons[cExp],tipe,nameMap,set[cId],cons[cDefn]) =>
    crFlow[cExp].
  liftExpCallOp(Lc,.vr(_,Nm,_),Args,Tp,Map,_,Ex) where _ ?= isEscape(Nm) =>
    (.cECall(Lc,Nm,Args,Tp),Ex).
  liftExpCallOp(Lc,.vr(_,Nm,_),Args,Tp,Map,_,Ex) where Entry ?= lookupVarName(Map,Nm) =>
    implementFunCall(Lc,Entry,Nm,Args,Tp,Map,Ex).
  liftExpCallOp(Lc,.enm(_,FullNm,_),Args,Tp,Map,_,Ex) => (.cTerm(Lc,FullNm,Args,Tp),Ex).
  liftExpCallOp(Lc,Op,Args,Tp,Map,Q,Ex) => valof{
--    logMsg("could not find $(Op) in $(Map)");
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
  implementFunCall(Lc,.localFun(Nm,ClNm,Th),_,Args,Tp,Map,Ex) => valof{
    V = liftVarExp(Lc,cName(Th),typeOf(Th),Map);
    valis (.cCall(Lc,Nm,[V,..Args],Tp),Ex)
  }
  implementFunCall(Lc,.labelArg(Base,Ix),_,Args,Tp,Map,Ex) => valof{
    V = liftVarExp(Lc,cName(Base),typeOf(Base),Map);
    valis (.cOCall(Lc,.cNth(Lc,V,Ix,Tp),Args,Tp),Ex)
  }
  implementFunCall(Lc,.memoArg(ClNm,Base,Ix,MTp),_,Args,Tp,Map,Ex) => valof{
    V = liftVarExp(Lc,cName(Base),typeOf(Base),Map);
    valis (.cOCall(Lc,.cThGet(Lc,.cNth(Lc,V,Ix,MTp),MTp),Args,Tp),Ex)
  }
  implementFunCall(Lc,.localVar(Vr),_,Args,Tp,Map,Ex) =>
    (.cOCall(Lc,Vr,Args,Tp),Ex).
  implementFunCall(Lc,.globalVar(Nm,GTp),_,Args,Tp,Map,Ex) =>
    (.cOCall(Lc,.cVar(Lc,.cId(Nm,GTp)),Args,Tp),Ex).
  implementFunCall(Lc,V,Vr,Args,Tp,Map,Ex) => valof{
    reportError("illegal variable $(Vr) - $(V)",Lc);
    valis (.cVoid(Lc,Tp),[])
  }

  liftLet:all e,x ~~ transform[e->>x],letify[x] |:
    (option[locn],cons[canonDef],cons[decl],e,nameMap,set[cId],set[cId],cons[cDefn]) =>
      crFlow[x].
  liftLet(Lc,Defs,Decls,Bnd,Outer,Q,Free,Ex) => valof{

--    logMsg("Q=$(Q), Free=$(Free)");
    (lVars,vrDefs) = unzip(varDefs(Defs));
    CM = makeConsMap(Decls);
    GrpFns = (Defs^/(D)=>~_?=isVarDef(D));

    rawGrpFree = freeLabelVars(Free,Outer)::cons[cId];

--    logMsg("cell vars $(lVars)");
    ffreeVars = rawGrpFree \ lVars;

--    logMsg("ffreeVars= $(ffreeVars), head outer map $(head(Outer))");
    varParents = freeParents(ffreeVars,Outer);
    freeVars = reduceFreeArgs(varParents,Outer);
    
    allFree = freeVars++lVars;

    if [SFr] .= allFree && isEmpty(lVars) then {
      MM = [.lyr(? SFr,foldRight((D,LL)=>collectMtd(D,.some(SFr),LL),[],GrpFns),CM),..Outer];
      M = Outer;
      GrpQ = foldLeft(collectQ,Q\+SFr,Defs);
      Ex1 = transformGroup(GrpFns,Outer,Outer,GrpQ,.some(.cVar(Lc,SFr)),Ex);
      valis transform(Bnd,MM,GrpQ,Ex1);
    } else {
      freeType = .tupleType(allFree//typeOf);

      ThV = genVar("_ThVr",freeType);
      ThVr = .cVar(Lc,ThV);

      L = collectLabelVars(allFree,ThV,0,[]);

      MM = [.lyr(.some(ThV),foldRight((D,LL)=>collectMtd(D,.some(ThV),LL),L,GrpFns),CM),..Outer];

      M = [.lyr(.some(ThV),L,CM),..Outer];

      GrpQ = foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q,lVars),Defs);
      
      Ex1 = transformGroup(GrpFns,M,M,GrpQ,.some(ThVr),Ex);
      
      freeArgs = (freeVars//(.cId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer));
      (cellArgs,Ex2) = liftExps(vrDefs,GrpQ,Outer,Ex1);

      GrpFree = crTpl(Lc,freeArgs++cellArgs);

      if traceNormalize! then{
	logMsg("free term is $(GrpFree)");
      };

      (BndTrm,Exx) = transform(Bnd,MM,GrpQ,Ex2);

      if traceNormalize! then{
	logMsg("definitions are $(Exx)");
      };

      valis (letify(Lc,ThV,GrpFree,BndTrm),Exx)
    }
  }
  -- In a let rec, all the non functions must end up in the free data

  liftLetRec:all e,x ~~ transform[e->>x],letify[x] |:
    (option[locn],cons[canonDef],cons[decl],e,nameMap,set[cId],set[cId],
      cons[cDefn]) => crFlow[x].
  liftLetRec(Lc,Grp,Decs,Bnd,Outer,Q,Free,Ex) => valof{
    GrpFns = (Grp^/(D)=>~_?=isVarDef(D));
    GrpVars = (Grp^/(D)=>_?=isVarDef(D));

    (lVars,vrDefs) = unzip(varDefs(GrpVars));

    rawGrpFree = freeLabelVars(Free,Outer)::cons[cId];
    varParents = freeParents(rawGrpFree \ lVars,Outer);
    freeVars = reduceFreeArgs(varParents,Outer);

    ThV = genVar("_ThVr",typeOf(freeVars++lVars));
    ThVr = .cVar(Lc,ThV);

    CM = makeConsMap(Decs);

    L = collectLabelVars(lVars,ThV,size(freeVars),collectLabelVars(freeVars,ThV,0,[]));

    if traceNormalize! then{
      logMsg("L = $(L)")
    };

    M = [.lyr(.some(ThV),foldRight((D,LL)=>collectMtd(D,.some(ThV),LL),L,GrpFns),CM),..Outer];

    freeArgs = (freeVars//(.cId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer));
    GrpQ = foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q\+ThV,lVars),Grp);

    cellVoids = (vrDefs//(E)=>.cVoid(Lc,typeOf(E)));
    GrpFree = crTpl(Lc,freeArgs++cellVoids);

    if traceNormalize! then{
      logMsg("lVars = $(lVars)");
      logMsg("vrDefs = $(vrDefs)");
      logMsg("GrpVars = $(GrpVars)");
      logMsg("GrpFree = $(GrpFree)");
    };

    if traceNormalize! then{
      logMsg("free term = $(GrpFree)")
    };
    (Fx,Ex2) = transformLetDefs(Grp,M,M,GrpQ,.some(ThVr),[],Ex);
    if traceNormalize! then{
      logMsg("fixups $(Fx)");
    };

    (BndTrm,Exx) = transform(Bnd,M,GrpQ,Ex2);
    valis (computeFixups(Fx,Lc,ThV,GrpFree,BndTrm),Exx)
  }

  fixUp ~> (string,integer,cExp).

  computeFixups:all e ~~ letify[e] |: (cons[fixUp],option[locn],cId,cExp,e) => e.
  computeFixups([],Lc,Vr,Fr,Bnd) => letify(Lc,Vr,Fr,Bnd).
  computeFixups([(Nm,Ix,Up),..Fx],Lc,Vr,Fr,Bnd) => valof{
    if traceNormalize! then{
      logMsg("compute fixup for $(Nm) at $(Ix) = $(Up)");
      logMsg("present: $(Vr) in $(Up) $(present(Up,(T) => (.cVar(_,VV).=T ?? VV==Vr || .false)))");
    };

    if ~present(Up,(T) => (.cVar(_,VV).=T ?? VV==Vr || .false)) &&
	.cTerm(FLc,FOp,FArgs,FTp) .= Fr then{
	  Fr1 = .cTerm(FLc,FOp,FArgs[Ix->Up],FTp);
	  if traceNormalize! then
	    logMsg("fixed up free term $(Fr1)");
	  valis computeFixups(Fx,Lc,Vr,Fr1,Bnd)
	}
    else{
      valis computeFixups(Fx,Lc,Vr,Fr,freeUpdate(Lc,.cVar(Lc,Vr),Ix,Up,Bnd))
    }
  }

  transformLetDefs:(cons[canonDef],nameMap,nameMap,set[cId],option[cExp],cons[fixUp],cons[cDefn]) =>
    (cons[fixUp],cons[cDefn]).
  transformLetDefs([],_,_,_,_,Fx,D) => (Fx,D).
  transformLetDefs([D,..Ds],Map,Outer,Q,Extra,Fx,Ex) => valof {
    (Fx1,Ex1) = transformLetDef(D,Map,Outer,Q,Extra,Fx,Ex);
    valis transformLetDefs(Ds,Map,Outer,Q,Extra,Fx1,Ex1)
  }

  transformLetDef:(canonDef,nameMap,nameMap,set[cId],option[cExp],cons[fixUp],cons[cDefn]) => (cons[fixUp],cons[cDefn]).
  transformLetDef(.varDef(Lc,FullNm,.lambda(_,_,Eqns,Tp),_,_),Map,Outer,Q,Extra,Fx,Ex) => valof{
    Ex1 = transformFunction(Lc,FullNm,Eqns,Tp,Map,Outer,Q,Extra,Ex);
    valis (Fx,Ex1)
  }
  transformLetDef(.varDef(Lc,FullNm,Val,Cx,Tp),Map,Outer,Q,.none,Fx,Ex) => valof{
    (Vl,Defs) = liftExp(Val,Outer,Q,Ex);
    valis (Fx,[.vrDef(Lc,FullNm,Tp,Vl),..Defs])
  }
  transformLetDef(.varDef(Lc,Nm,Val,Cx,Tp),Map,Outer,Q,.some(V),Fx,Ex) where .cVar(VLc,ThVr) .= V => valof{
    if traceNormalize! then
      logMsg("lift $(Nm)\:$(Tp) = $(Val) @ $(Lc)");

    if (_,Ix) ?= labelIndex(Nm,Map) then{
      (Vl,Defs) = liftExp(Val,Outer,Q,Ex);
      valis ([(Nm,Ix,Vl),..Fx],Defs)
    }
    else{
      reportError("cannot compile var $(Nm), because not in map",Lc);
      valis (Fx,Ex)
    }
  }
  transformLetDef(.implDef(Lc,_,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Fx,Ex) =>
    transformLetDef(.varDef(Lc,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Fx,Ex).
  transformLetDef(.typeDef(Lc,Nm,Tp,TpRl),Map,_,_,_,Fx,Ex) => valof{
    Ex1 = transformTypeDef(Lc,Nm,Tp,TpRl,Map,Ex);
    valis (Fx,Ex1)
  }
  transformLetDef(.cnsDef(Lc,Nm,FullNm,Tp),Map,_,_,_,Fx,Ex) => valof{
    Ex1 = transformConsDef(Lc,FullNm,Tp,Map,Ex);
    valis (Fx,Ex1)
  }
  transformLetDef(.conDef(_,_,_,_),_,_,_,_,Fx,Ex) => (Fx,Ex).
  transformLetDef(.accDef(_,_,_,_),_,_,_,_,Fx,Ex) => (Fx,Ex).
  transformLetDef(.updDef(_,_,_,_),_,_,_,_,Fx,Ex) => (Fx,Ex).

  liftAction:(canonAction,nameMap,set[cId],cons[cDefn]) => (aAction,cons[cDefn]).
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
  liftAction(.doRaise(Lc,T,E),Map,Q,Ex) => valof{
    (TT,Ex1) = liftExp(T,Map,Q,Ex);
    (EE,Ex2) = liftExp(E,Map,Q,Ex1);
    valis (.aRaise(Lc,TT,EE),Ex1)
  }
  liftAction(.doDefn(Lc,P,E),Map,Q,Ex) => valof{
    (PP,Ex1) = liftPtn(P,Map,Q,Ex);
    (EE,Ex2) = liftExp(E,Map,Q,Ex1);
    valis (.aDefn(Lc,PP,EE),Ex2)
  }
  liftAction(.doMatch(Lc,P,E),Map,Q,Ex) => valof{
    (PP,Ex1) = liftPtn(P,Map,Q,Ex);
    (EE,Ex2) = liftExp(E,Map,Q,Ex1);
    valis (.aDefn(Lc,PP,EE),Ex2)
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
    (CCs,Ex2) = transformRules(Cs,Map,Q,.none,Ex1);
    if .cVar(_,_).=LGv then{
      Reslt = caseMatcher(Lc,Map,LGv,.aAbort(Lc,"no matches"),CCs);

      valis (Reslt,Ex2)
    } else {
      V = genVar("C",typeOf(Gv));
      Res = caseMatcher(Lc,Map,.cVar(Lc,V),.aAbort(Lc,"no matches"),CCs);
      valis (.aLtt(Lc,V,LGv,Res),Ex2)
    }
  }
  liftAction(.doTryCatch(Lc,B,Th,H),Map,Q,Ex) => valof{
    if ErTp?=getTypeArg(typeOf(Th),"star.core*cont") then{
      (TT,Ex1) = liftPtn(Th,Map,Q,Ex);
      (BB,Ex2) = liftAction(B,Map,ptnVars(Th,Q,[]),Ex1);
      (Hs,Ex3) = transformRules(H,Map,Q,.none,Ex2);
      ErrVr = .cVar(Lc,genVar("E",ErTp));
      Hndlr = caseMatcher(Lc,Map,ErrVr,.aAbort(Lc,"no matches"),Hs);
      
      valis (.aTry(Lc,BB,TT,ErrVr,Hndlr),Ex3)
    }
    else {
      reportError("expecting a continuation type, not $(typeOf(Th))",Lc);
      valis (.aNop(Lc),Ex)
    }
  }
  liftAction(.doCall(Lc,E),Map,Q,Ex) => valof{
    (EE,Ex1) = liftExp(E,Map,Q,Ex);
    valis (.aPerf(Lc,EE),Ex1)
  }
  liftAction(.doLet(Lc,Grp,Dcs,Bnd),Map,Q,Ex) => valof{
    Free = findFree(.doLet(Lc,Grp,Dcs,Bnd),Q);
    valis liftLet(Lc,Grp,Dcs,Bnd,Map,Q,Free,Ex)
  }
  liftAction(.doLetRec(Lc,Grp,Dcs,Bnd),Map,Q,Ex) => valof{
    Free = findFree(.doLetRec(Lc,Grp,Dcs,Bnd),Q);
    valis liftLetRec(Lc,Grp,Dcs,Bnd,Map,Q,Free,Ex)
  }
  
  varDefs:(cons[canonDef]) => cons[(cId,canon)].
  varDefs(Defs) =>
    foldLeft((D,FF) => (V?=isVarDef(D) ?? [V,..FF] || FF),
      [],Defs).

  isVarDef(.varDef(_,FullNm,Vl,_,Tp)) where ~isFunDef(Vl) =>
    .some((.cId(FullNm,Tp),Vl)).
  isVarDef(.implDef(_,_,Nm,Vl,_,Tp)) where ~isFunDef(Vl) =>
    .some((.cId(Nm,Tp),Vl)).
  isVarDef(_) default => .none.

  collectLabelVars:(cons[cId],cId,integer,map[string,nameMapEntry]) =>
    map[string,nameMapEntry].
  collectLabelVars([],_,_,LV) => LV.
  collectLabelVars([.cId(Nm,Tp),..Vrs],ThV,Ix,Entries) =>
    collectLabelVars(Vrs,ThV,Ix+1,Entries[Nm->.labelArg(ThV,Ix)]).
  
  collectVars:(cons[canonDef],cId,integer,map[string,nameMapEntry]) =>
    map[string,nameMapEntry].
  collectVars([],_,_,LV) => LV.
  collectVars([.varDef(Lc,FullNm,_,_,Tp),..Vrs],ThV,Ix,Entries) =>
    collectVars(Vrs,ThV,Ix+1,Entries[FullNm->.memoArg(varClosureNm(FullNm),ThV,Ix,Tp)]).
  collectVars([.implDef(Lc,_,FullNm,_,_,Tp),..Vrs],ThV,Ix,Entries) =>
    collectVars(Vrs,ThV,Ix+1,Entries[FullNm->.memoArg(varClosureNm(FullNm),ThV,Ix,Tp)]).

  -- eliminate free variables that can be computed from other free vars
  reduceFreeArgs:(cons[cId],nameMap) => cons[cId].
  reduceFreeArgs(FrVrs,Map) => let{.
    reduceArgs:(cons[cId],cons[cId]) => cons[cId].
    reduceArgs([],Frs) => Frs.
    reduceArgs([FrV,..FrArgs],Frs) where
	FrNm .= cName(FrV) &&
	OTh ?= lookupThetaVar(Map,FrNm) &&
	OTh .<. Frs =>
      reduceArgs(FrArgs,drop(FrV,Frs)).
    reduceArgs([_,..FrArgs],Frs) => reduceArgs(FrArgs,Frs).
  .} in reduceArgs(FrVrs,FrVrs).

  freeParents:(cons[cId],nameMap) => cons[cId].
  freeParents(Frs,Map) => foldLeft((F,Fs)=>Fs\+freeParent(F,Map),[],Frs).

  freeParent(V,Map) where ThV ?= lookupThetaVar(Map,cName(V)) =>
    freeParent(ThV,Map).
  freeParent(V,_) default => V.

  collectMtd:(canonDef,option[cId],map[string,nameMapEntry])=>map[string,nameMapEntry].

  collectMtd(.varDef(Lc,Nm,Val,_,Tp),.some(ThVr),LL) where isFunDef(Val) =>
    LL[Nm->.localFun(Nm,closureNm(Nm),ThVr)].
  collectMtd(.varDef(Lc,Nm,Val,_,Tp),.none,LL) where isFunDef(Val) =>
    LL[Nm->.moduleFun(.cTerm(Lc,closureNm(Nm),[crTpl(Lc,[])],Tp),Nm)].
  collectMtd(.varDef(Lc,Nm,Val,_,Tp),.none,LL) =>
    LL[Nm->.globalVar(Nm,Tp)].
  collectMtd(.varDef(Lc,Nm,Val,_,Tp),.some(ThVr),LL) => LL.
  collectMtd(.varDef(Lc,Nm,Val,_,Tp),.some(ThVr),LL) =>
    LL[Nm->.localVar(.cCall(Lc,varClosureNm(Nm),[.cVar(Lc,ThVr)],Tp))].
  collectMtd(.implDef(Lc,_,FullNm,Val,Cx,Tp),ThVr,LL) =>
    collectMtd(.varDef(Lc,FullNm,Val,Cx,Tp),ThVr,LL).
  collectMtd(.cnsDef(Lc,Nm,FullNm,Tp),.none,LL) => LL[Nm->.moduleCons(FullNm,Tp)].
  collectMtd(.cnsDef(Lc,Nm,FullNm,Tp),.some(ThVr),LL) => LL[Nm->.localCons(FullNm,Tp,ThVr)].
  collectMtd(.typeDef(_,_,_,_),_,LL) => LL.
  collectMtd(.conDef(_,_,_,_),_,LL) => LL.

  collectQ:(canonDef,set[cId]) => set[cId].
  collectQ(.varDef(Lc,Nm,Val,_,Tp),Q) => Q\+.cId(Nm,Tp).
  collectQ(.implDef(Lc,_,FullNm,Val,_,Tp),Q) => Q\+.cId(FullNm,Tp).
  collectQ(.cnsDef(_,Nm,FullNm,Tp),Q) => Q.
  collectQ(.typeDef(_,_,_,_),Q) => Q.
  collectQ(.conDef(_,_,_,_),Q) => Q.

  freeLabelVars:(set[cId],nameMap)=>set[cId].
  freeLabelVars(Fr,Map) => foldLeft((V,So)=>labelVar(V,Map,So),Fr,Fr).

  labelVar:(cId,nameMap,set[cId])=>set[cId].
  labelVar(.cId(Nm,_),Map,So) where Entry?=lookupVarName(Map,Nm) =>
    case Entry in {
      .labelArg(ThVr,_) => So\+ThVr.
      .localFun(_,_,ThVr) => So\+ThVr.
      _ => So
    }.
  labelVar(_,_,So) default => So.

  labelIndex:(string,nameMap) => option[(cId,integer)].
  labelIndex(Nm,Map) => valof{
    if E?=lookupVarName(Map,Nm) then{
      case E in {
	.labelArg(Thv,Ix) => valis ? (Thv,Ix).
	_ default => valis .none
      }
    } else
    valis .none
  }

  makeFunVars:(tipe)=>cons[cId].
  makeFunVars(Tp) where .tupleType(Es).=funTypeArg(deRef(Tp)) => (Es//(E)=>genVar("_",E)).

  genVar:(string,tipe)=>cId.
  genVar(Pr,Tp) => .cId(genId(Pr),Tp).
}
