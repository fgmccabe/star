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

  public normalize:(pkgSpec,cons[canonDef],cons[decl])=>cons[crDefn].
  normalize(PkgSpec,Defs,Decls) => valof{
    Map = pkgMap(PkgSpec,Decls);
    if traceNormalize! then
      logMsg("package map $(Map)");
    valis transformGroup(Defs,Map,Map,[],.none,[],Rp)
  }

  transformGroup:(cons[canonDef],nameMap,nameMap,set[cId],option[crExp],cons[crDefn]) =>
    cons[crDefn].
  transformGroup([],_,_,_,_,D) => D.
  transformGroup([D,..Ds],Map,Outer,Q,Extra,Ex) => valof {
    Ex1 = transformDef(D,Map,Outer,Q,Extra,Ex);
    valis transformGroup(Ds,Map,Outer,Q,Extra,Ex1)
  }

  transformDef:(canonDef,nameMap,nameMap,set[cId],option[cExp],cons[crDefn]) =>
    cons[crDefn].
  transformDef(varDef(Lc,Nm,FullNm,lambda(_,LNm,Eqns,Tp),_),Map,Outer,Q,Extra,Ex) => valof{
    if traceNormalize! then
      logMsg("transform $(lambda(Lc,LNm,Eqns,Tp))");
    ATp = extendFunTp(deRef(Tp),Extra);
    (Eqs,Ex1) = transformEquations(Eqns,Outer,Q,Extra,Ex);
    if traceNormalize! then
      logMsg("transformed $(Eqns) equations: $(Eqs)");
    Func = functionMatcher(Lc,FullNm,ATp,Map,Eqs);
    if traceNormalize! then
      logMsg("transformed function $(Func)");

    ClosureNm = closureNm(FullNm);
    ClVar = (cVar(_,Exv)^=Extra ? Exv || cId("_",unitTp));
    ClVars = makeFunVars(Tp);
    ClArgs = [ClVar,..ClVars];

    ClosTp = extendFunTp(deRef(Tp),some(ClVar));

    if Exv^=Extra then {
      ClosEntry =
	fnDef(Lc,ClosureNm,ClosTp,ClArgs,
	  cCall(Lc,FullNm,ClArgs//(V)=>cVar(Lc,V),funTypeRes(Tp)));
      valis [Func,ClosEntry,..Ex1]
    } else {
      ClosEntry =
	fnDef(Lc,ClosureNm,ClosTp,
	  ClArgs,cCall(Lc,FullNm,ClVars//(V)=>cVar(Lc,V),funTypeRes(Tp)));
      valis [Func,ClosEntry,..Ex1]
    }
  }
  transformDef(varDef(Lc,_,FullNm,Val,Cx,Tp),Map,Outer,Q,.none,Ex) => valof{
    (Vl,Defs) = liftExp(Val,Outer,Q,Ex);
    valis [vrDef(Lc,FullNm,Tp,Vl),..Defs]
  }
  transformDef(varDef(Lc,Nm,FullNm,Val,Cx,Tp),Map,Outer,Q,some(cVar(VLc,ThVr)),Ex) => valof{
    ThIx ^= findMemoIx(Nm,ThVr,Map);
    (Vl,Defs) <- liftExp(Val,Outer,Q,Ex,Rp);
    Body .= crTplOff(Lc,crCnd(Lc,crMatch(Lc,crVoid(Lc,Tp),crTplOff(Lc,V,ThIx,Tp)),
	crTplUpdate(Lc,V,ThIx,Vl),V),ThIx,Tp);
    ClosureNm .= varClosureNm(FullNm);
    ClosEntry .= fnDef(Lc,ClosureNm,funType([typeOf(ThVr)],Tp),[ThVr],Body);
    valis [ClosEntry,..Defs]
  }
  transformDef(implDef(Lc,_,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Ex,Rp) => 
    transformDef(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),Map,Outer,Q,Extra,Ex,Rp).
  transformDef(typeDef(Lc,Nm,Tp,TpRl),Map,_,_,_,Ex,Rp) =>
    transformTypeDef(Lc,Nm,Tp,TpRl,Map,Ex,Rp).
  transformDef(cnsDef(Lc,Nm,FullNm,Tp),Map,_,_,_,Ex,Rp) =>
    transformConsDef(Lc,FullNm,Tp,Map,Ex,Rp).
  transformDef(conDef(_,_,_,_),_,_,_,_,Ex,_) => do{
    valis Ex
  }.
  transformDef(implDef(_,_,_,_,_,_),_,_,_,_,Ex,_) => do{
    valis Ex
  }.
  transformDef(accDef(_,_,_,_),_,_,_,_,Ex,_) => do{
    valis Ex
  }.
  transformDef(updDef(_,_,_,_),_,_,_,_,Ex,_) => do{
    valis Ex
  }.

  transformTypeDef(Lc,Nm,Tp,TpRl,Map,Ex,Rp) => do{
--    logMsg("look for $(Nm)\:$(Tp) in $(Map)");
    ConsMap ^= findIndexMap(Nm,Map);
--    logMsg("constructor map $(ConsMap)");
    valis [tpDef(Lc,Tp,TpRl,ConsMap),..Ex]
  }

  transformConsDef(Lc,Nm,Tp,Map,Ex,Rp) => do{
--    logMsg("look for $(Nm)\:$(Tp) constructor");
    (_,CT) .= deQuant(Tp);
    (_,IT) .= deConstrain(CT);
    (ATp,RTp) ^= isConsType(IT);
    if (Ar,_) ^= isTupleType(ATp) then{
--    logMsg("look for $(tpName(RTp)) type: $(findIndexMap(tpName(RTp),Map))");
      ConsMap ^= findIndexMap(tpName(RTp),Map);
--      logMsg("consmap for $(Nm)\:$(ConsMap)");
      (Lbl,Ix) ^= findLbl(Nm,ConsMap);
      valis [lblDef(Lc,Lbl,Tp,Ix),..Ex]
    }
    else{
--      logMsg("ignore $(Nm)");
      valis Ex
    }
  }

  findLbl(Nm,[]) => .none.
  findLbl(Nm,[(tLbl(Nm,Ar),_,Ix),.._]) => some((tLbl(Nm,Ar),Ix)).
  findLbl(Nm,[_,..Ms]) => findLbl(Nm,Ms).
    

  transformEquations:(cons[equation],nameMap,set[cId],option[cExp],cons[crDefn],reports) =>
    result[reports,(cons[(option[locn],cons[cExp],option[cExp],cExp)],cons[crDefn])].
  transformEquations([],_,_,_,Ex,_) => do{
    valis ([],Ex)
  }
  transformEquations([Eqn,..Eqns],Map,Q,Extra,Ex,Rp) => do{
--    logMsg("transform equation $(Eqn)");
    (Trple,Ex1) <- transformEquation(Eqn,Map,Q,Extra,Ex,Rp);
    (Rest,Exx) <- transformEquations(Eqns,Map,Q,Extra,Ex1,Rp);
    valis ([Trple,..Rest],Exx)
  }

  transformEquation:(equation,nameMap,set[cId],option[cExp],cons[crDefn],reports) =>
    result[reports,((option[locn],cons[cExp],option[cExp],cExp),cons[crDefn])].
  transformEquation(eqn(Lc,tple(ALc,As),.none,Val),Map,Q,Extra,Ex,Rp) => do{
    EQ .= ptnVars(tple(ALc,As),Q,[]);
    (Ptns,Ex1) <- liftPtns(As,Map,Q,Ex,Rp);
--    logMsg("patterns $(As) lifted to $(Ptns)");
    (Rep,Exx) <- liftExp(Val,Map,EQ,Ex1,Rp);
    valis ((Lc,addExtra(Extra,Ptns),.none,Rep),Exx)
  }
  transformEquation(eqn(Lc,tple(ALc,As),some(Wh),Val),Map,Q,Extra,Ex,Rp) => do{
    (Ptns,Ex1) <- liftPtns(As,Map,Q,Ex,Rp);
    EQ .= ptnVars(tple(Lc,As),Q,[]);
    (Cond,Ex2) <- liftGoal(Wh,Map,EQ,Ex1,Rp);
    GLQ .= glVars(Wh,EQ);
    (Rep,Exx) <- liftExp(Val,Map,GLQ,Ex2,Rp);

    valis ((Lc,addExtra(Extra,Ptns),some(Cond),Rep),Exx)
  }
  
  addExtra(.none,Args) => Args.
  addExtra(some(P),Args) => [P,..Args].

  liftPtn:(canon,nameMap,set[cId],cons[crDefn]) => crFlow.
  liftPtn(vr(Lc,Nm,Tp),Map,_,Ex) => trVarPtn(Lc,Nm,Tp,Map,Ex).
  liftPtn(enm(Lc,FullNm,Tp),Map,_,Ex) => (cTerm(Lc,FullNm,[],Tp),Ex).
  liftPtn(intr(Lc,Ix),Map,_,Ex) =>  (cInt(Lc,Ix),Ex).
  liftPtn(bintr(Lc,Ix),Map,_,Ex) => (cBig(Lc,Ix),Ex).
  liftPtn(flt(Lc,Dx),Map,_,Ex) => (cFloat(Lc,Dx),Ex).
  liftPtn(kar(Lc,Cx),Map,_,Ex) => (cChar(Lc,Cx),Ex).
  liftPtn(strng(Lc,Sx),Map,_,Ex) => (cString(Lc,Sx),Ex).
  liftPtn(whr(Lc,Ptn,Cond),Map,Q,Ex) => valof{
    (LPtn,Ex1) = liftPtn(Ptn,Map,Q,Ex);
    (LCond,Exx) = liftGoal(Cond,Map,ptnVars(Ptn,Q,[]),Ex);
    valis (cWhere(Lc,LPtn,LCond),Exx)
  }
  liftPtn(tple(Lc,Els),Map,Q,Ex) => valof{
    (LEls,Exx) = liftPtns(Els,Map,Q,Ex);
    valis (crTpl(Lc,LEls),Exx)
  }
  liftPtn(apply(Lc,vr(VLc,VNm,_),tple(_,Els),Tp),Map,Q,Ex) => valof{
    (LArgs,Ex1) = liftPtns(Els,Map,Q,Ex);
    valis liftPtnCallOp(Lc,VNm,LArgs,Tp,Map,Q,Ex1)
  }
  liftPtn(apply(Lc,enm(VLc,FullNm,_),tple(_,Els),Tp),Map,Q,Ex) => valof{
    (LArgs,Ex1) = liftPtns(Els,Map,Q,Ex,Rp);

    valis (cTerm(Lc,FullNm,LArgs,Tp),Ex1)
  }
  liftPtn(Cn,_,_,Ex) => valof{
    Lc = locOf(Cn);
    reportError("may not have $(Cn) as a pattern",Lc);
    valis (cVoid(Lc,typeOf(Cn)),Ex)
  }
  
  liftPtns:(cons[canon],nameMap,set[cId],cons[crDefn]) => (cons[cExp],cons[crDefn]).
  liftPtns([],_,_,Ex) => ([],Ex).
  liftPtns([P,..Ps],Map,Q,Ex) => valof{
    (A,Ex1) = liftPtn(P,Map,Q,Ex);
    (As,Exx) = liftPtns(Ps,Map,Q,Ex1);
    valis ([A,..As],Exx)
  }

  liftPtnCallOp:(option[locn],string,cons[cExp],tipe,nameMap,set[cId],cons[crDefn]) =>
    (cExp,cons[crDefn]).
  liftPtnCallOp(Lc,Nm,Args,Tp,Map,Q,Ex) where Entry^= lookupVarName(Map,Nm) =>
    implementPtnCall(Lc,Entry,Args,Tp,Map,Q,Ex).

  implementPtnCall(Lc,moduleCons(Nm,CTp),Args,Tp,_,_,Ex) =>
    (cTerm(Lc,Nm,Args,Tp),Ex).
  implementPtnCall(Lc,localCons(Nm,CTp,Vr),Args,Tp,_,_,Ex) =>
    (cTerm(Lc,Nm,[cVar(Lc,Vr),..Args],Tp),Ex).
  
  trVarPtn(Lc,Nm,Tp,Map,Ex) =>
    implementVarPtn(Lc,Nm,lookupVarName(Map,Nm),Tp,Map,Ex).

  implementVarPtn(Lc,Nm,.none,Tp,_,Ex) => (cVar(Lc,cId(Nm,Tp)),Ex).
  implementVarPtn(Lc,Nm,some(moduleCons(Enum,CTp)),Tp,_,Ex) where ETp^=isEnumType(CTp) =>
    (crTerm(Lc,Enum,[],ETp),Ex).
  implementVarPtn(Lc,Nm,some(localCons(Enum,CTp,Vr)),Tp,_,Ex) =>
    (crTerm(Lc,Enum,[cVar(Lc,Vr)],Tp),Ex).
  implementVarPtn(Lc,Nm,some(labelArg(Base,Ix)),Tp,Map,Ex) => valof{
    V = liftVarExp(Lc,crName(Base),typeOf(Base),Map);
    NN = cVar(Lc,cId(Nm,Tp));
    valis (crWhere(Lc,NN,crMatch(Lc,NN,crTplOff(Lc,V,Ix,Tp))),Ex)
  }
  implementVarPtn(Lc,Nm,some(memoArg(ClNm,Base,Ix)),Tp,Map,Ex) => valof{
    V = liftVarExp(Lc,crName(Base),typeOf(Base),Map);
    NN = cVar(Lc,cId(Nm,Tp));
    valis (crWhere(Lc,NN,crMatch(Lc,NN,cCall(Lc,ClNm,[V],Tp))),Ex)
  }
  implementVarPtn(Lc,Nm,some(V),Tp,_Map,Ex) => valof{
    reportError("not permitted to match against $(Nm)\:$(V)",Lc);
    valid (cVoid(Lc,Tp),Ex)
  }.

  liftExp:(canon,nameMap,set[cId],cons[crDefn]) => crFlow.
  liftExp(vr(Lc,Nm,Tp),Map,Q,Ex) => valof{
    VV = liftVarExp(Lc,Nm,Tp,Map);
    valis (VV,Ex)
  }
  liftExp(intr(Lc,Ix),_,Map,Ex) => (cInt(Lc,Ix),Ex).
  liftExp(bintr(Lc,Ix),_,Map,Ex,Rp) => (cBig(Lc,Ix),Ex).
  liftExp(flt(Lc,Dx),_,Map,Ex,Rp) => (cFloat(Lc,Dx),Ex).
  liftExp(kar(Lc,Cx),_,Map,Ex,Rp) => (cChar(Lc,Cx),Ex).
  liftExp(strng(Lc,Sx),_,Map,Ex,Rp) => (cString(Lc,Sx),Ex).
  liftExp(enm(Lc,FullNm,Tp),_,Map,Ex,Rp) => (cTerm(Lc,FullNm,[],Tp),Ex).
  liftExp(tple(Lc,Els),Q,Map,Ex,Rp) => do{
    (LEls,Exx) <- liftExps(Els,Map,Q,Ex,Rp);
    valis (mcTpl(Lc,LEls),Exx)
  }
  liftExp(apply(Lc,Op,tple(_,Els),Tp),Q,Map,Ex,Rp) => do{
    (LEls,Ex1) <- liftExps(Els,Map,Q,Ex,Rp);
    liftExpCallOp(Lc,Op,LEls,Tp,Q,Map,Ex1,Rp)
  }
  liftExp(dot(Lc,Rc,Fld,Tp),Map,Q,Ex,Rp) => do{
    raise reportError(Rp,"unexpected dot expression $(dot(Lc,Rc,Fld,Tp))",Lc)
  }
  liftExp(whr(_,E,enm(_,"star.core#true",_)),Map,Q,Ex,Rp) =>
    liftExp(E,Map,Q,Ex,Rp).
  liftExp(whr(Lc,E,C),Map,Q,Ex,Rp) => do{
    (LE,Ex1) <- liftExp(E,Map,Q,Ex,Rp);
    (LC,Ex2) <- liftGoal(C,Map,Q,Ex1,Rp);
    valis (crWhere(Lc,LE,LC),Ex2)
  }
  liftExp(E,Map,Q,Ex,Rp) where isGoal(E) => liftGoal(E,Map,Q,Ex,Rp).
  liftExp(cond(Lc,Ts,Th,El),Map,Q,Ex,Rp) => do{
    (LTs,Ex1) <- liftGoal(Ts,Map,Q,Ex,Rp);
    Q1 .= glVars(Ts,Q);
    (LTh,Ex2) <- liftExp(Th,Map,Q1,Ex1,Rp);
    (LEl,Exx) <- liftExp(El,Map,Q,Ex2,Rp);
    valis (crCnd(Lc,LTs,LTh,LEl),Exx)
  }
  liftExp(letExp(Lc,Grp,Decs,Bnd),Map,Q,Ex,Rp) => 
    liftLetExp(Lc,Grp,Decs,Bnd,Map,Q,Ex,Rp).
  liftExp(letRec(Lc,Grp,Decs,Bnd),Map,Q,Ex,Rp) => 
    liftLetRec(Lc,Grp,Decs,Bnd,Map,Q,Ex,Rp).
  liftExp(lambda(Lc,FullNm,Eqns,Tp),Map,Q,Ex,Rp) => do{
    liftExp(letExp(Lc,[varDef(Lc,FullNm,FullNm,lambda(Lc,FullNm,Eqns,Tp),[],Tp)],
	[funDec(Lc,FullNm,FullNm,Tp)],
	vr(Lc,FullNm,Tp)),Map,Q,Ex,Rp)
  }
  liftExp(csexp(Lc,Gov,Cses,Tp),Map,Q,Ex,Rp) => do{
    (LGov,Ex1) <- liftExp(Gov,Map,Q,Ex,Rp);
    (Cs,Ex2) <- transformEquations(Cses,Map,Q,.none,Ex1,Rp);
    if cVar(_,_).=LGov then{
      Reslt .= caseMatcher(Lc,Map,LGov,Cs,Tp);
      valis (Reslt,Ex2)
    } else {
      V .= genVar("C",typeOf(LGov));
      Res .= caseMatcher(Lc,Map,cVar(Lc,V),Cs,Tp);
      valis (crLtt(Lc,V,LGov,Res),Ex2)
    }
  }

  liftExps:(cons[canon],set[cId],nameMap,cons[crDefn],reports) =>
    result[reports,(cons[cExp],cons[crDefn])].
  liftExps([],_,_,Ex,_) => do{ valis ([],Ex)}.
  liftExps([P,..Ps],Q,Map,Ex,Rp) => do{
    (A,Ex1) <- liftExp(P,Map,Q,Ex,Rp);
    (As,Exx) <- liftExps(Ps,Q,Map,Ex1,Rp);
    valis ([A,..As],Exx)
  }

  liftVarExp:(option[locn],string,tipe,nameMap,reports) => result[reports,cExp].
  liftVarExp(Lc,Nm,Tp,Map,Rp) where Entry ^= lookupVarName(Map,Nm) =>
    implementVarExp(Lc,Entry,Map,Tp,Rp).
  liftVarExp(Lc,Nm,Tp,Map,Rp) => do{
    valis cVar(Lc,cId(Nm,Tp))
  }

  implementVarExp:(option[locn],nameMapEntry,nameMap,tipe,reports) => result[reports,cExp].
  implementVarExp(Lc,localFun(_,ClNm,ThVr),Map,Tp,Rp) => do{
    V <- liftVarExp(Lc,crName(ThVr),Tp,Map,Rp);
    valis crTerm(Lc,ClNm,[V],Tp)
  }
  implementVarExp(Lc,labelArg(Base,Ix),Map,Tp,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    valis crTplOff(Lc,V,Ix,Tp)
  }.
  implementVarExp(Lc,memoArg(ClNm,Base,Ix),Map,Tp,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    valis cCall(Lc,ClNm,[V],Tp)
  }
  implementVarExp(Lc,localVar(Vr),_,Tp,Rp) => do{ valis Vr}.
  implementVarExp(Lc,moduleCons(Enum,CTp),_,Tp,Rp) => do{ valis crTerm(Lc,Enum,[],Tp)}.
  implementVarExp(Lc,localCons(Enum,CTp,Vr),Map,Tp,Rp) => do{
    V <- liftVarExp(Lc,crName(Vr),typeOf(Vr),Map,Rp);
    valis crTerm(Lc,Enum,[V],Tp)
  }
  implementVarExp(Lc,moduleFun(V,_),_,Tp,Rp) => do{ valis V}.
  implementVarExp(Lc,globalVar(Nm,GTp),_,Tp,Rp) => do{ valis cVar(Lc,cId(Nm,GTp))}.
  implementVarExp(Lc,E,_,_,Rp) =>
    do{
      raise reportError(Rp,"cannot transform variable $(E)",Lc)
    }.

  liftExpCallOp:(option[locn],canon,cons[cExp],tipe,nameMap,set[cId],cons[crDefn],reports) =>
    result[reports,crFlow].
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,_,Ex,Rp) where _ ^= isEscape(Nm) =>
    do{ valis (crECall(Lc,Nm,Args,Tp),Ex)}.
  liftExpCallOp(Lc,vr(_,Nm,_),Args,Tp,Map,_,Ex,Rp) where Entry ^= lookupVarName(Map,Nm) =>
    implementFunCall(Lc,Entry,Nm,Args,Tp,Map,Ex,Rp).
  liftExpCallOp(Lc,enm(_,FullNm,_),Args,Tp,Map,_,Ex,Rp) =>
    do{ valis (crTerm(Lc,FullNm,Args,Tp),Ex)}.
  liftExpCallOp(Lc,Op,Args,Tp,Map,Q,Ex,Rp) => do{
    (LOp,Ex0) <- liftExp(Op,Map,Q,Ex,Rp);
    valis (crOCall(Lc,LOp,Args,Tp),Ex0)
  }
  liftExpCallOp(Lc,Op,Args,Tp,_,_,_,Rp) => do{
    raise reportError(Rp,"cannot compile function $(Op) applied to $(Args)",Lc)
  }.

  implementFunCall:(option[locn],nameMapEntry,string,cons[cExp],tipe,nameMap,cons[crDefn],reports) =>
    result[reports,crFlow].
  implementFunCall(Lc,moduleFun(_,Fn),_,Args,Tp,Map,Ex,Rp) =>
    do{ valis (cCall(Lc,Fn,Args,Tp),Ex)}.
  implementFunCall(Lc,moduleCons(Fn,FTp),_,Args,Tp,Map,Ex,Rp) =>
    do{ valis (crTerm(Lc,Fn,Args,Tp),Ex)}.
  implementFunCall(Lc,localCons(Fn,FTp,Vr),_,Args,Tp,Map,Ex,Rp) => do{
    VV<-liftVarExp(Lc,crName(Vr),typeOf(Vr),Map,Rp);
    valis (crTerm(Lc,Fn,[VV,..Args],Tp),Ex)
  }
  implementFunCall(Lc,localFun(Nm,ClNm,Th),_,Args,Tp,Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,crName(Th),Tp,Map,Rp);
    valis (cCall(Lc,Nm,[V,..Args],Tp),Ex)
  }
  implementFunCall(Lc,labelArg(Base,Ix),_,Args,Tp,Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),typeOf(Base),Map,Rp);
    valis (crOCall(Lc,crTplOff(Lc,V,Ix,Tp),Args,Tp),Ex)
  }
  implementFunCall(Lc,memoArg(ClNm,Base,Ix),_,Args,Tp,Map,Ex,Rp) => do{
    V <- liftVarExp(Lc,crName(Base),Tp,Map,Rp);
    valis (crOCall(Lc,cCall(Lc,ClNm,[V],funType([typeOf(V)],Tp)),Args,Tp),Ex)
  }
  implementFunCall(Lc,localVar(Vr),_,Args,Tp,Map,Ex,Rp) =>
    do{ valis (crOCall(Lc,Vr,Args,Tp),Ex)}.
  implementFunCall(Lc,globalVar(Nm,GTp),_,Args,Tp,Map,Ex,Rp) =>
    do{ valis (crOCall(Lc,cVar(Lc,cId(Nm,GTp)),Args,Tp),Ex)}.
  implementFunCall(Lc,V,Vr,Args,Tp,Map,Ex,Rp) =>
    do{
      raise reportError(Rp,"illegal variable $(Vr) - $(V)",Lc)
    }.

  liftGoal:(canon,nameMap,set[cId],cons[crDefn],reports) => result[reports,crFlow].
  liftGoal(conj(Lc,L,R),Map,Q,Ex,Rp) => do{
    (LL,Ex1) <- liftGoal(L,Map,Q,Ex,Rp);
    (LR,Ex2) <- liftGoal(R,Map,glVars(L,Q),Ex1,Rp);
    valis (crCnj(Lc,LL,LR),Ex2)
  }
  liftGoal(disj(Lc,L,R),Map,Q,Ex,Rp) => do{
    (LL,Ex1) <- liftGoal(L,Map,Q,Ex,Rp);
    (LR,Ex2) <- liftGoal(R,Map,Q,Ex1,Rp);
    valis (crDsj(Lc,LL,LR),Ex2)
  }
  liftGoal(neg(Lc,R),Map,Q,Ex,Rp) => do{
    (LR,Ex1) <- liftGoal(R,Map,Q,Ex,Rp);
    valis (crNeg(Lc,LR),Ex1)
  }
  liftGoal(cond(Lc,T,L,R),Map,Q,Ex,Rp) => do{
    (LT,Ex1) <- liftGoal(T,Map,Q,Ex,Rp);
    Q1 .= glVars(T,Q);
    (LL,Ex2) <- liftGoal(L,Map,Q1,Ex1,Rp);
    (LR,Ex3) <- liftGoal(R,Map,Q,Ex2,Rp);
    valis (crCnd(Lc,LT,LL,LR),Ex3)
  }
  liftGoal(match(Lc,P,E),Map,Q,Ex,Rp) => do{
    (LP,Ex1) <- liftPtn(P,Map,Q,Ex,Rp);
    (LE,Ex2) <- liftExp(E,Map,Q,Ex1,Rp);
    valis (crMatch(Lc,LP,LE),Ex2)
  }
  liftGoal(G,Map,Q,Ex,Rp) =>
    liftExp(G,Map,Q,Ex,Rp).

  liftLetExp:(option[locn],cons[canonDef],cons[decl],
    canon,nameMap,set[cId],cons[crDefn],reports) =>
    result[reports,crFlow].
  liftLetExp(Lc,Defs,Decls,Bnd,Outer,Q,Ex,Rp) => do{
--    logMsg("lift let group $(Defs) @ $(Lc)");
--    logMsg("Q=$(Q)");

    (lVars,vrDefs) .= unzip(varDefs(Defs));
    CM .= makeConsMap(Decls);
    GrpFns .= (Defs^/(D)=>~_^=isVarDef(D));

    rawGrpFree .= freeLabelVars(freeVarsInTerm(letExp(Lc,Defs,Decls,Bnd),[],Q,[]),Outer)::cons[cId];

--    logMsg("cell vars $(lVars)");
    ffreeVars .= rawGrpFree \ lVars;

--    logMsg("simplifying $(ffreeVars)");
    varParents .= freeParents(ffreeVars,Outer);
    freeVars <- reduceFreeArgs(varParents,Outer,Rp);
--    logMsg("simplified free $(freeVars)\:$(typeOf(freeVars))");
    
    allFree .= freeVars++lVars;

    if [SFr] .= allFree && isEmpty(lVars) then {
      M .= [lyr(.none,foldRight((D,LL)=>collectMtd(D,some(SFr),LL),[],GrpFns),CM),..Outer];
      GrpQ .= foldLeft(collectQ,[SFr,..Q],Defs);
      Ex1 <- transformGroup(GrpFns,M,Outer,GrpQ,some(cVar(Lc,SFr)),Ex,Rp);
      liftExp(Bnd,M,GrpQ,Ex1,Rp);
    } else {
      freeType .= tupleType(allFree//typeOf);

      ThV .= genVar("_ThVr",freeType);
      ThVr .= cVar(Lc,ThV);

      L .= collectLabelVars(allFree,ThV,0,[]);

      MM .= [lyr(some(ThV),foldRight((D,LL)=>collectMtd(D,some(ThV),LL),L,GrpFns),CM),..Outer];
    
--      logMsg("theta var $(ThV) ~ $(L)");
    
      M .= [lyr(some(ThV),L,CM),..Outer];

--      logMsg("let map is $(head(MM))");

      GrpQ .= foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q,lVars),Defs);
      
      Ex1 <- transformGroup(GrpFns,M,M,GrpQ,some(ThVr),Ex,Rp);
      
      freeArgs <- seqmap((cId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer,Rp),freeVars);
      (cellArgs,Ex2) <- liftExps(vrDefs,GrpQ,Outer,Ex1,Rp);

      GrpFree .= crTpl(Lc,freeArgs++cellArgs);

--      logMsg("free data $(ThV) = $(GrpFree)\:$(typeOf(GrpFree))");
      (BndTrm,Exx) <- liftExp(Bnd,MM,GrpQ,Ex2,Rp);
      valis (crLtt(Lc,ThV,GrpFree,BndTrm),Exx)
    }
  }

  varDefs:(cons[canonDef]) => cons[(cId,canon)].
  varDefs(Defs) =>
    foldLeft((D,FF) => (V^=isVarDef(D) ? [V,..FF] || FF),
      [],Defs).

  isVarDef(varDef(_,Nm,FullNm,Vl,_,Tp)) where ~isFunDef(Vl) =>
    some((cId(Nm,Tp),Vl)).
  isVarDef(implDef(_,_,Nm,Vl,_,Tp)) where ~isFunDef(Vl) =>
    some((cId(Nm,Tp),Vl)).
  isVarDef(_) default => .none.

  collectLabelVars:(cons[cId],cId,integer,map[string,nameMapEntry]) =>
    map[string,nameMapEntry].
  collectLabelVars([],_,_,LV) => LV.
  collectLabelVars([V,..Vrs],ThV,Ix,Entries) where cId(Nm,Tp) .= V =>
    collectLabelVars(Vrs,ThV,Ix+1,Entries[Nm->labelArg(ThV,Ix)]).
  

  -- In a let rec, all the non functions must end up in the free data

  liftLetRec:(option[locn],cons[canonDef],cons[decl],canon,nameMap,set[cId],
    cons[crDefn],reports) => result[reports,crFlow].
  liftLetRec(Lc,Grp,Decs,Bnd,Outer,Q,Ex,Rp) => do{
--    logMsg("lift let rec group $(Grp) @ $(Lc)");
--    logMsg("Q=$(Q)");
    GrpFns .= (Grp^/(D)=>~_^=isVarDef(D));
    GrpVars .= (Grp^/(D)=>_^=isVarDef(D));
    (lVars,vrDefs) .= unzip(varDefs(Grp));
--    logMsg("lVars = $(lVars)");
--    logMsg("vrDefs = $(vrDefs)");
    
    rawGrpFree .= freeLabelVars(freeVarsInTerm(letRec(Lc,Grp,Decs,Bnd),[],Q,[]),Outer)::cons[cId];
--    logMsg("raw free vars $(rawGrpFree)");
    varParents .= freeParents(rawGrpFree \ lVars,Outer);
    freeVars <- reduceFreeArgs(varParents,Outer,Rp);
--    logMsg("free variables $(freeVars)\:$(typeOf(freeVars))");
--    logMsg("layer var $(layerVar(Outer))");

    ThV .= genVar("_ThVr",typeOf(freeVars++lVars));
    ThVr .= cVar(Lc,ThV);

    CM .= makeConsMap(Decs);

    L .= collectVars(GrpVars,ThV,size(freeVars),collectLabelVars(freeVars,ThV,0,[]));
    M .= [lyr(some(ThV),foldRight((D,LL)=>collectMtd(D,some(ThV),LL),L,GrpFns),CM),..Outer];

--      logMsg("theta var $(ThV)\:$(typeOf(ThV)) ~ $(L)");
--      logMsg("letrec map is $(head(M))");

    freeArgs <- seqmap((cId(VNm,VTp))=>liftVarExp(Lc,VNm,VTp,Outer,Rp),freeVars);
--      logMsg("free vars lift to $(freeArgs)");
    cellVoids .= (vrDefs//(E)=>crVoid(Lc,typeOf(E)));
    GrpFree .= crTpl(Lc,freeArgs++cellVoids);
      
--      logMsg("free data $(ThV) = $(GrpFree)\:$(typeOf(GrpFree))");

    GrpQ .= foldLeft(collectQ,foldLeft((V,QQ)=>QQ\+V,Q\+ThV,lVars),Grp);
    Ex2 <- transformGroup(Grp,M,M,GrpQ,some(ThVr),Ex,Rp);
      
    (BndTrm,Exx) <- liftExp(Bnd,M,GrpQ,Ex2,Rp);
    valis (crLtt(Lc,ThV,GrpFree,BndTrm),Exx)
  }

  collectVars:(cons[canonDef],cId,integer,map[string,nameMapEntry]) =>
    map[string,nameMapEntry].
  collectVars([],_,_,LV) => LV.
  collectVars([varDef(Lc,Nm,FullNm,_,_,Tp),..Vrs],ThV,Ix,Entries) =>
    collectVars(Vrs,ThV,Ix+1,Entries[Nm->memoArg(varClosureNm(FullNm),ThV,Ix)]).
  collectVars([implDef(Lc,_,FullNm,_,_,Tp),..Vrs],ThV,Ix,Entries) =>
    collectVars(Vrs,ThV,Ix+1,Entries[FullNm->memoArg(varClosureNm(FullNm),ThV,Ix)]).


  -- eliminate free variables that can be computed from other free vars
  reduceFreeArgs:(cons[cId],nameMap,reports) => result[reports,cons[cId]].
  reduceFreeArgs(FrVrs,Map,Rp) => let{.
    reduceArgs:(cons[cId],cons[cId]) => result[reports,cons[cId]].
    reduceArgs([],Frs) => do{ valis Frs}.
    reduceArgs([FrV,..FrArgs],Frs) where
	FrNm .= crName(FrV) &&
	OTh ^= lookupThetaVar(Map,FrNm) &&
	OTh .<. Frs =>
      reduceArgs(FrArgs,drop(FrV,Frs)).
    reduceArgs([_,..FrArgs],Frs) => reduceArgs(FrArgs,Frs).
  .} in reduceArgs(FrVrs,FrVrs).

  freeParents:(cons[cId],nameMap) => cons[cId].
  freeParents(Frs,Map) => foldLeft((F,Fs)=>Fs\+freeParent(F,Map),[],Frs).

  freeParent(V,Map) where ThV ^= lookupThetaVar(Map,crName(V)) =>
    freeParent(ThV,Map).
  freeParent(V,_) default => V.

  collectMtd:(canonDef,option[cId],map[string,nameMapEntry])=>map[string,nameMapEntry].

  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),some(ThVr),LL) where isFunDef(Val) =>
    LL[Nm->localFun(FullNm,closureNm(FullNm),ThVr)].
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),.none,LL) where isFunDef(Val) =>
    LL[Nm->moduleFun(crTerm(Lc,closureNm(FullNm),[crTpl(Lc,[])],Tp),FullNm)].
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),.none,LL) =>
    LL[Nm->globalVar(FullNm,Tp)].
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),some(ThVr),LL) => LL.
  collectMtd(varDef(Lc,Nm,FullNm,Val,_,Tp),some(ThVr),LL) =>
    LL[Nm->localVar(cCall(Lc,varClosureNm(FullNm),[cVar(Lc,ThVr)],Tp))].
  collectMtd(implDef(Lc,_,FullNm,Val,Cx,Tp),ThVr,LL) =>
    collectMtd(varDef(Lc,FullNm,FullNm,Val,Cx,Tp),ThVr,LL).
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),.none,LL) => LL[Nm->moduleCons(FullNm,Tp)].
  collectMtd(cnsDef(Lc,Nm,FullNm,Tp),some(ThVr),LL) => LL[Nm->localCons(FullNm,Tp,ThVr)].
  collectMtd(typeDef(_,_,_,_),_,LL) => LL.
  collectMtd(conDef(_,_,_,_),_,LL) => LL.

  collectQ:(canonDef,set[cId]) => set[cId].
  collectQ(varDef(Lc,Nm,FullNm,Val,_,Tp),Q) => Q\+cId(Nm,Tp).
  collectQ(implDef(Lc,_,FullNm,Val,_,Tp),Q) => Q\+cId(FullNm,Tp).
  collectQ(cnsDef(_,Nm,FullNm,Tp),Q) => Q.
  collectQ(typeDef(_,_,_,_),Q) => Q.
  collectQ(conDef(_,_,_,_),Q) => Q.

  freeLabelVars:(set[cId],nameMap)=>set[cId].
  freeLabelVars(Fr,Map) => foldLeft((V,So)=>labelVar(V,Map,So),Fr,Fr).

  labelVar(cId(Nm,_),Map,So) where Entry^=lookupVarName(Map,Nm) =>
    case Entry in {
      labelArg(ThVr,_) => So\+ThVr.
      localFun(_,_,ThVr) => So\+ThVr.
      _ => So
    }.
  labelVar(_,_,So) default => So.

  genVar:(string,tipe) => cId.
  genVar(Pr,Tp) => cId(genSym(Pr),Tp).

  makeFunVars:(tipe)=>cons[cId].
  makeFunVars(Tp) where tupleType(Es).=funTypeArg(deRef(Tp)) => (Es//(E)=>genVar("_",E)).

  crTpl:(option[locn],cons[cExp]) => cExp.
  crTpl(Lc,Args) => let{
    Tp = typeOf(Args).
    Ar = size(Args).
  } in crTerm(Lc,tplLbl(Ar),Args,Tp).

  closureNm:(string)=>string.
  closureNm(Nm)=>Nm++"^".

  varClosureNm:(string)=>string.
  varClosureNm(Nm) => Nm++"$".
}
