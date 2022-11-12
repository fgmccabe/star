star.compiler.resolve{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.unify.

  public overloadProgram:(cons[cons[canonDef]],dict) => cons[cons[canonDef]].
  overloadProgram(Gps,Dict) => overloadGroups(Gps,[],Dict).

  overloadGroups:(cons[cons[canonDef]],cons[cons[canonDef]],dict) =>
    cons[cons[canonDef]].
  overloadGroups([],Gps,_) => reverse(Gps).
  overloadGroups([Gp,..Gps],RG,Dict) => valof{
    (RGp,NDict) = overloadGroup(Gp,Dict);
    valis overloadGroups(Gps,[RGp,..RG],NDict)
  }

  overloadGroup:(cons[canonDef],dict)=>(cons[canonDef],dict).
  overloadGroup(Dfs,Dict) => overloadDefs(Dfs,Dict,[]).

  overloadDefs:(cons[canonDef],dict,cons[canonDef]) => (cons[canonDef],dict).
  overloadDefs([],Dict,Dfx) => (reverse(Dfx),Dict).
  overloadDefs([D,..Defs],Dict,Dfx) => valof{
--    logMsg("overload definition $(D)");
    (DD,DDict) = overloadDef(D,Dict);
--    logMsg("overloaded definition $(DD)");
    valis overloadDefs(Defs,DDict,[DD,..Dfx])
  }

  overloadDef:(canonDef,dict)=>(canonDef,dict).
  overloadDef(.varDef(Lc,Nm,FullNm,Val,Cx,Tp),Dict) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp).
  overloadDef(.implDef(Lc,Nm,FullNm,Val,Cx,Tp),Dict) =>
    overloadImplDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp).
  overloadDef(.typeDef(Lc,Nm,Tp,TpRl),Dict) => (.typeDef(Lc,Nm,Tp,TpRl),Dict).
  overloadDef(.conDef(Lc,Nm,Tp,TpRl),Dict) => (.conDef(Lc,Nm,Tp,TpRl),Dict).
  overloadDef(.cnsDef(Lc,Nm,FullNm,Tp),Dict) => (.cnsDef(Lc,Nm,FullNm,Tp),Dict).
  overloadDef(Def,Dict) default => valof{
    reportError("cannot overload $(Def)",locOf(Def));
    valis (Def,Dict)
  }
 
  overloadVarDef:(dict,option[locn],string,string,canon,cons[constraint],tipe)=>
    (canonDef,dict).
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,[],Tp) => 
    (varDef(Lc,Nm,FullNm,resolveTerm(Val,Dict),[],Tp),Dict).
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp) => valof{
--    logMsg("overload $(FullNm) = $(Val)");
    (Cvrs,CDict) = defineCVars(Lc,Cx,[],Dict);
    RVal = resolveTerm(Val,CDict);
    (Qx,Qt) = deQuant(Tp);
    (_,ITp) = deConstrain(Qt);
    CTp = reQuant(Qx,funType(Cx//typeOf,ITp));
    valis (trace varDef(Lc,Nm,FullNm,lambda(Lc,lambdaLbl(Lc),[rule(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
  }

  overloadImplDef:(dict,option[locn],string,string,canon,cons[constraint],tipe) =>
    (canonDef,dict).
  overloadImplDef(Dict,Lc,Nm,FullNm,Val,_,Tp) => valof{
--    logMsg("overload implementation $(Nm) = $(Val)\:$(Tp)");
    
    (Qx,Qt) = deQuant(Tp);
    (Cx,ITp) = deConstrain(Qt);

--    logMsg("constraints $(Cx)");

    (Cvrs,CDict) = defineCVars(Lc,Cx,[],Dict);

--    logMsg("cvars = $(Cvrs)");
    RVal = resolveTerm(Val,CDict);

    if isEmpty(Cvrs) then {
      CTp = reQuant(Qx,ITp);
      valis (implDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
    } else {
      CTp = reQuant(Qx,funType(Cx//genContractType,ITp));
      valis (implDef(Lc,Nm,FullNm,lambda(Lc,lambdaLbl(Lc),[rule(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
    }
  }

  genContractType(.conTract(Nm,Tps,Dps)) => mkConType(Nm,Tps,Dps).

  defineCVars:(option[locn],cons[constraint],cons[canon],dict) => (cons[canon],dict).
  defineCVars(_,[],Vrs,D) => (reverse(Vrs),D).
  defineCVars(Lc,[T,..Tps],Vrs,D) where TpNm .= implementationName(T) && Tp.=typeOf(T) =>
    defineCVars(Lc,Tps,[vr(Lc,TpNm,Tp),..Vrs],
      declareVar(TpNm,Lc,Tp,.none,
	declareImplementation(Lc,TpNm,TpNm,Tp,D))).

  resolveTerm:(canon,dict) => canon.
  resolveTerm(C,D) => resolveAgain(.inactive,C,overloadTerm(C,D,.inactive),D).

  resolveAgain(_,_,(T,.resolved),D) =>
    resolveAgain(.inactive,T,overloadTerm(T,D,.inactive),D).
  resolveAgain(_,_,(T,.inactive),_) => T.
  resolveAgain(.active(_,Msg),_,(T,.active(Lc,Msg)),_) => valof{
    reportError(Msg,Lc);
    valis T
  }
  resolveAgain(_,O,(_,.active(Lc,Msg)),D) =>
    resolveAgain(.active(Lc,Msg),O,overloadTerm(O,D,.inactive),D).

  overloadTerm:(canon,dict,resolveState) => (canon,resolveState).
  overloadTerm(.anon(Lc,Tp),_,St) => (.anon(Lc,Tp),St).
  overloadTerm(.vr(Lc,Nm,Tp),_,St) => (.vr(Lc,Nm,Tp),St).
  overloadTerm(.intr(Lc,Ix),_,St) => (.intr(Lc,Ix),St).
  overloadTerm(.bintr(Lc,Ix),_,St) => (.bintr(Lc,Ix),St).
  overloadTerm(.flt(Lc,Dx),_,St) => (.flt(Lc,Dx),St).
  overloadTerm(.kar(Lc,Cx),_,St) => (.kar(Lc,Cx),St).
  overloadTerm(.strng(Lc,Sx),_,St) => (.strng(Lc,Sx),St).
  overloadTerm(.enm(Lc,FullNm,Tp),_,St) => (.enm(Lc,FullNm,Tp),St).
  overloadTerm(.dot(Lc,Rc,Fld,Tp),Dict,St) => valof{
    (Rc1,St1) = overloadTerm(Rc,Dict,St);
    valis resolveDot(Lc,Rc1,Fld,Tp,Dict,St1);
  }
  overloadTerm(.update(Lc,R,F,V),Dict,St) => valof{
    (OR,St1) = overloadTerm(R,Dict,St);
    (OV,St2) = overloadTerm(V,Dict,St1);
    valis resolveUpdate(Lc,OR,F,OV,Dict,St2);
  }
  overloadTerm(.tple(Lc,Els),Dict,St) => valof{
    (REls,St1) = overloadTplEls(Els,Dict,St);
    valis (.tple(Lc,REls),St1)
  }
  overloadTerm(.owpen(Lc,Rc),Dict,St) => valof{
    (Rc1,St1) = overloadTerm(Rc,Dict,St);
    valis (.owpen(Lc,Rc1),St1);
  }
  overloadTerm(.whr(Lc,T,C),Dict,St) => valof{
    (OT,St1) = overloadTerm(T,Dict,St);
    (OC,St2) = overloadTerm(C,Dict,St1);
    valis (.whr(Lc,OT,OC),St2)
  }
  overloadTerm(.mtd(Lc,Nm,Tp),Dict,St) => 
    (.mtd(Lc,Nm,Tp),.active(Lc,"cannot resolve unconstrained method #(Nm)\:$(Tp)")).
  overloadTerm(.over(Lc,T,Cx),Dict,St) => valof{
--    logMsg("over $(over(Lc,T,Cx))");
    (DArgs,St1) = resolveContracts(Lc,Cx,[],Dict,St);
    (OverOp,NArgs,St2) = resolveRef(T,DArgs,[],Dict,St1);
    valis (overApply(Lc,OverOp,NArgs,typeOf(T)),markResolved(St2))
  }
  overloadTerm(.overaccess(Lc,T,RcTp,Fld,FldTp),Dict,St) => valof{
--    logMsg("over access $(T)");
    if (AccessOp,St1) ?= resolveAccess(Lc,RcTp,Fld,FldTp,Dict,St) then{
      (OverOp,NArgs,St2) = resolveRef(T,[AccessOp],[],Dict,St1);
      valis (curryOver(Lc,OverOp,NArgs,funType([RcTp],FldTp)),St2)
    } else{
      valis (.overaccess(Lc,T,RcTp,Fld,FldTp),.active(Lc,"cannot find accessor for #(Fld)"))
    }
  }
  overloadTerm(.apply(lc,.over(OLc,T,Cx),Args,Tp),Dict,St) => valof{
    (DArgs,St1) = resolveContracts(OLc,Cx,[],Dict,St);
    (RArgs,St2) = overloadTplEls(Args,Dict,St1);
    (OverOp,NArgs,St3) = resolveRef(T,DArgs,RArgs,Dict,St2);
    valis (.apply(lc,OverOp,NArgs,Tp),markResolved(St3))
  }
  overloadTerm(.apply(lc,.overaccess(Lc,T,RcTp,Fld,FldTp),Args,Tp),Dict,St) => valof{
    if (AccessOp,St1) ?= resolveAccess(Lc,RcTp,Fld,FldTp,Dict,St) then{
      (RArgs,St2) = overloadTplEls(Args,Dict,St1);
      (OverOp,NArgs,St3) = resolveRef(T,[AccessOp],RArgs,Dict,St2);
      valis (.apply(lc,OverOp,NArgs,Tp),St3)
    } else {
      valis (.apply(lc,.overaccess(Lc,T,RcTp,Fld,FldTp),Args,Tp),
	.active(Lc,"cannot find accessor for #(Fld)"))
    }
  }
  overloadTerm(.apply(lc,Op,Args,Tp),Dict,St) => valof{
    (ROp,St1) = overloadTerm(Op,Dict,St);
    (RArgs,St2) = overloadTplEls(Args,Dict,St1);
    valis (.apply(lc,ROp,RArgs,Tp),St2)
  }
  overloadTerm(.match(Lc,Ptn,Src),Dict,St) => valof{
    (RPtn,St1) = overloadTerm(Ptn,Dict,St);
    (RSrc,St2) = overloadTerm(Src,Dict,St1);
    valis (.match(Lc,RPtn,RSrc),St2)
  }
  overloadTerm(.conj(Lc,Lhs,Rhs),Dict,St) => valof{
    (RLhs,St1) = overloadTerm(Lhs,Dict,St);
    (RRhs,St2) = overloadTerm(Rhs,Dict,St1);
    valis (.conj(Lc,RLhs,RRhs),St2)
  }
  overloadTerm(.disj(Lc,Lhs,Rhs),Dict,St) => valof{
    (RLhs,St1) = overloadTerm(Lhs,Dict,St);
    (RRhs,St2) = overloadTerm(Rhs,Dict,St1);
    valis (.disj(Lc,RLhs,RRhs),St2)
  }
  overloadTerm(.neg(Lc,Rhs),Dict,St) => valof{
    (RRhs,St1) = overloadTerm(Rhs,Dict,St);
    valis (.neg(Lc,RRhs),St1)
  }
  overloadTerm(.cond(Lc,Tst,Lhs,Rhs),Dict,St) => valof{
    (RTst,St1) = overloadTerm(Tst,Dict,St);
    (RLhs,St2) = overloadTerm(Lhs,Dict,St1);
    (RRhs,St3) = overloadTerm(Rhs,Dict,St2);
    valis (.cond(Lc,RTst,RLhs,RRhs),St3)
  }
  overloadTerm(.lambda(Lc,Nm,Rls,Tp),Dict,St) => valof{
    (RRls,St1) = overloadRules(Rls,[],overloadTerm,Dict,St);
    valis (.lambda(Lc,Nm,RRls,Tp),St1)
  }
  overloadTerm(.letExp(Lc,Gp,Decls,Rhs),Dict,St) => valof{
    (RDfs,_) = overloadGroup(Gp,Dict);
    (RRhs,St1) = overloadTerm(Rhs,declareDecls(Decls,Dict),St);
    valis (.letExp(Lc,RDfs,Decls,RRhs),St1)
  }
  overloadTerm(.letRec(Lc,Gp,Decs,Rhs),Dict,St) => valof{
    (RDfs,RDct) = overloadGroup(Gp,declareDecls(Decs,Dict));
    (RRhs,St2) = overloadTerm(Rhs,RDct,St);
    valis (.letRec(Lc,RDfs,Decs,RRhs),St2)
  }
  overloadTerm(.csexp(Lc,Gov,Cases,Tp),Dict,St) => valof{
    (RGov,St1) = overloadTerm(Gov,Dict,St);
    (RCases,St2) = overloadRules(Cases,[],overloadTerm,Dict,St1);
    valis (.csexp(Lc,RGov,RCases,Tp),St2)
  }
  overloadTerm(.trycatch(Lc,A,H,Tp),Dict,St) => valof{
    (AA,St1) = overloadTerm(A,Dict,St);
    (HH,St2) = overloadTerm(H,Dict,St1);
    valis (.trycatch(Lc,AA,HH,Tp),St2)
  }
  overloadTerm(.vlof(Lc,Act,Tp),Dict,St) => valof{
    (Ac,St1) = overloadAction(Act,Dict,St);
    valis (.vlof(Lc,Ac,Tp),St1)
  }
  overloadTerm(T,_,St) => valof{
    Lc = locOf(T);
    reportError("cannot resolve term $(T)",Lc);
    valis (T,.active(Lc,"cannot resolve term $(T)"))
  }
  
  resolveRef(.mtd(Lc,Nm,Tp),[DT,..Ds],Args,Dict,St) => valof{
    (OverOp,St1) = resolveDot(Lc,DT,Nm,Tp,Dict,St);
    valis (OverOp,Ds++Args,St1)
  }
  resolveRef(C,DArgs,Args,_,St) default =>
    (C,DArgs++Args,St).

  overApply(_,OverOp,[],_) => OverOp.
  overApply(Lc,OverOp,Args,Tp) where ~ _ ?= isFunType(Tp) =>
    apply(Lc,OverOp,Args,Tp).
  overApply(Lc,OverOp,Args,Tp) =>
    curryOver(Lc,OverOp,Args,Tp).

  curryOver(Lc,OverOp,Args,Tp) where .tupleType(ArgTps) .= funTypeArg(Tp) => valof{
    Vrs = { .vr(Lc,genSym("A"),ArgTp) | ArgTp in ArgTps};
    NArgs = Args++Vrs;
    valis .lambda(Lc,lambdaLbl(Lc),
      [.rule(Lc,.tple(Lc,Vrs),.none,.apply(Lc,OverOp,NArgs,Tp))],
      funType(ArgTps,Tp))
  }

  overloadAction(.doNop(Lc),_,St) => (doNop(Lc),St).
  overloadAction(.doSeq(Lc,L,R),Dict,St) => valof{
    (LL,St1) = overloadAction(L,Dict,St);
    (RR,St2) = overloadAction(R,Dict,St1);
    valis (.doSeq(Lc,LL,RR),St2)
  }
  overloadAction(.doLbld(Lc,Lb,A),Dict,St) => valof{
    (AA,St1) = overloadAction(A,Dict,St);
    valis (.doLbld(Lc,Lb,AA),St1)
  }
  overloadAction(.doBrk(Lc,Lb),Dict,St) => (.doBrk(Lc,Lb),St).
  overloadAction(.doValis(Lc,E),Dict,St) => valof{
    (EE,St1) = overloadTerm(E,Dict,St);
    valis (.doValis(Lc,EE),St1)
  }
  overloadAction(.doThrow(Lc,E),Dict,St) => valof{
    (EE,St1) = overloadTerm(E,Dict,St);
    valis (.doThrow(Lc,EE),St1)
  }
  overloadAction(.doDefn(Lc,P,V),Dict,St) => valof{
    (PP,St1) = overloadTerm(P,Dict,St);
    (VV,St2) = overloadTerm(V,Dict,St1);
    valis (.doDefn(Lc,PP,VV),St2)
  }
  overloadAction(.doMatch(Lc,P,V),Dict,St) => valof{
    (PP,St1) = overloadTerm(P,Dict,St);
    (VV,St2) = overloadTerm(V,Dict,St1);
    valis (.doMatch(Lc,PP,VV),St2)
  }
  overloadAction(.doAssign(Lc,P,V),Dict,St) => valof{
    (PP,St1) = overloadTerm(P,Dict,St);
    (VV,St2) = overloadTerm(V,Dict,St1);
    valis (.doAssign(Lc,PP,VV),St2)
  }
  overloadAction(.doTryCatch(Lc,A,H),Dict,St) => valof{
    (AA,St1) = overloadAction(A,Dict,St);
    (HH,St2) = overloadRules(H,[],overloadAction,Dict,St1);
    valis (.doTryCatch(Lc,AA,HH),St2)
  }
  overloadAction(.doIfThen(Lc,T,Th,El),Dict,St) => valof{
    (TT,St1) = overloadTerm(T,Dict,St);
    (TTh,St2) = overloadAction(Th,Dict,St1);
    (EEl,St3) = overloadAction(El,Dict,St2);
    valis (.doIfThen(Lc,TT,TTh,EEl),St3)
  }
  overloadAction(.doWhile(Lc,T,A),Dict,St) => valof{
    (TT,St1) = overloadTerm(T,Dict,St);
    (AA,St2) = overloadAction(A,Dict,St1);
    valis (.doWhile(Lc,TT,AA),St2)
  }
  overloadAction(.doLet(Lc,Gp,Decls,B),Dict,St) => valof{
    (RDfs,_) = overloadGroup(Gp,Dict);
    (BB,St1) = overloadAction(B,declareDecls(Decls,Dict),St);
    valis (.doLet(Lc,RDfs,Decls,BB),St1)
  }
  overloadAction(.doLetRec(Lc,Gp,Decs,B),Dict,St) => valof{
    (RDfs,RDct) = overloadGroup(Gp,declareDecls(Decs,Dict));
    (BB,St2) = overloadAction(B,RDct,St);
    valis (.doLetRec(Lc,RDfs,Decs,BB),St2)
  }
  overloadAction(.doCase(Lc,A,H),Dict,St) => valof{
    (AA,St1) = overloadTerm(A,Dict,St);
    (HH,St2) = overloadRules(H,[],overloadAction,Dict,St1);
    valis (.doCase(Lc,AA,HH),St2)
  }
  overloadAction(.doSuspend(Lc,T,E,H),Dict,St) => valof{
    (TT,St1) = overloadTerm(T,Dict,St);
    (EE,St2) = overloadTerm(E,Dict,St1);
    (HH,St3) = overloadRules(H,[],overloadAction,Dict,St2);
    valis (.doSuspend(Lc,TT,EE,HH),St3)
  }
  overloadAction(.doResume(Lc,T,E,H),Dict,St) => valof{
    (TT,St1) = overloadTerm(T,Dict,St);
    (EE,St2) = overloadTerm(E,Dict,St1);
    (HH,St3) = overloadRules(H,[],overloadAction,Dict,St2);
    valis (.doResume(Lc,TT,EE,HH),St3)
  }
  overloadAction(.doRetire(Lc,T,E),Dict,St) => valof{
    (TT,St1) = overloadTerm(T,Dict,St);
    (EE,St2) = overloadTerm(E,Dict,St1);
    valis (.doRetire(Lc,TT,EE),St2)
  }
  overloadAction(.doCall(Lc,E),Dict,St) => valof{
    (EE,St1) = overloadTerm(E,Dict,St);
    valis (.doCall(Lc,EE),St1)
  }
    
  /*
  
    ([A,..Args],St1) .= resolveContracts(Lc,Cx,[],Dict,St);
--    logMsg("contract args $([A,..Args]), St1=$(St1)");
    if mtd(_,Nm,_,MTp) .= T then{
      if _eof(Args) then
	valis overloadTerm(dot(Lc,A,Nm,MTp),Dict,St1)
      else
      valis overloadTerm(apply(Lc,dot(Lc,A,Nm,MTp),Args,typeOf(T)),Dict,St1)
    }
    else{
      (Tr,St2) = overloadTerm(T,Dict,St1);
      valis (apply(Lc,Tr,[A,..Args],typeOf(T)),St2)
    }
  }
*/
    
  overloadRules:all x ~~ (cons[rule[x]],cons[rule[x]],
    (x,dict,resolveState)=>(x,resolveState),dict,resolveState)=>
    (cons[rule[x]],resolveState).
  overloadRules([],Els,_,Dict,St) => (reverse(Els),St).
  overloadRules([.rule(Lc,Ptn,.none,Exp),..Ts],Els,H,Dict,St) => valof{
    (RPtn,St1) = overloadTerm(Ptn,Dict,St);
    (RExp,St2) = H(Exp,Dict,St1);
    valis overloadRules(Ts,[.rule(Lc,RPtn,.none,RExp),..Els],H,Dict,St2)
  }
  overloadRules([.rule(Lc,Ptn,.some(Wh),Exp),..Ts],Els,H,Dict,St) => valof{
    (RPtn,St1) = overloadTerm(Ptn,Dict,St);
    (RExp,St2) = H(Exp,Dict,St1);
    (RWh,St3) = overloadTerm(Wh,Dict,St2);
    valis overloadRules(Ts,[.rule(Lc,RPtn,.some(RWh),RExp),..Els],H,Dict,St3)
  }
  
  overloadTerms([],Els,Dict,St) => (reverse(Els),St).
  overloadTerms([T,..Ts],Els,Dict,St) => valof{
    (RT,St1) = overloadTerm(T,Dict,St);
    valis overloadTerms(Ts,[RT,..Els],Dict,St1)
  }

  overloadTplEls(Els,Dict,St) => overloadTerms(Els,[],Dict,St).
    
  resolveContracts:(option[locn],cons[constraint],cons[canon],dict,resolveState) =>
    (cons[canon],resolveState).
  resolveContracts(_,[],Cx,_,St) => (reverse(Cx),St).
  resolveContracts(Lc,[C,..Cx],Vs,Dict,St) => valof{
    (A,St1) = resolveContract(Lc,C,Dict,St);
    valis resolveContracts(Lc,Cx,[A,..Vs],Dict,St1)
  }
  
  resolveContract:(option[locn],constraint,dict,resolveState) => (canon,resolveState).
  resolveContract(Lc,Con,Dict,St) => valof{
--    logMsg("resolve contract $(Con)");
    ImpNm = implementationName(Con);
    Tp = typeOf(Con);
    if Impl?=findImplementation(Dict,ImpNm) then {
--      logMsg("resolve contract $(Con) using $(Impl)\:$(typeOf(Impl))");
      if sameType(typeOf(Impl),Tp,Dict) then {
--	logMsg("resolving impl var $(Impl)");
	valis overloadTerm(Impl,Dict,markResolved(St))
      } else{
	valis (.anon(Lc,Tp),.active(Lc,"implementation $(typeOf(Impl)) not consistent with $(Tp)"))
      }
    } else{
      valis (.anon(Lc,Tp),.active(Lc,"cannot find an implementation for $(Tp)"))
    }
  }

  resolveAccess:(option[locn],tipe,string,tipe,dict,resolveState) => option[(canon,resolveState)].
  resolveAccess(Lc,RcTp,Fld,Tp,Dict,St) => valof{
--    logMsg("resolve access $(RcTp).$(Fld)\:$(Tp)");
    if AccFn ?= findAccess(Lc,RcTp,Fld,Dict) then{
--      logMsg("access fun $(AccFn)\:$(typeOf(AccFn))");
      Ft = newTypeVar("F");
      if sameType(typeOf(AccFn),funType([RcTp],Ft),Dict) then{
	if sameType(Tp,snd(freshen(Ft,Dict)),Dict) then{
	  valis .some((AccFn,markResolved(St)))
	} else{
	  valis .none
	}
      } else {
	valis .none
      }
    } else{
      valis .none
    }
  }

  resolveDot:(option[locn],canon,string,tipe,dict,resolveState) => (canon,resolveState).
  resolveDot(Lc,Rc,Fld,Tp,Dict,St) => valof{
--    logMsg("resolve access at $(Lc) of $(Rc).$(Fld), expected type $(Tp)");
    RcTp = typeOf(Rc);
    if AccFn ?= findAccess(Lc,RcTp,Fld,Dict) then{
--      logMsg("access fun $(AccFn)\:$(typeOf(AccFn))");
      Ft = newTypeVar("F");
      if sameType(typeOf(AccFn),funType([RcTp],Ft),Dict) then{
	if sameType(Tp,snd(freshen(Ft,Dict)),Dict) then{
	  valis overloadTerm(apply(Lc,AccFn,[Rc],Tp),Dict,markResolved(St))
	} else{
	  valis (dot(Lc,Rc,Fld,Tp),
	    .active(Lc,"field $(Rc).$(Fld)\:$(Ft) not consistent with required type $(Tp)")).
	}
      } else {
	valis (dot(Lc,Rc,Fld,Tp),
	  .active(Lc,"accessor for field $(Rc).$(Fld)\:$(typeOf(AccFn)) for $(RcTp) not consistent with required type $(Tp)"))
      }
    } else{
      valis (dot(Lc,Rc,Fld,Tp),
	.active(Lc,"cannot find accessor for field $(Fld) for $(RcTp)"))
    }
  }

  resolveUpdate:(option[locn],canon,string,canon,dict,resolveState) => (canon,resolveState).
  resolveUpdate(Lc,Rc,Fld,Vl,Dict,St) => valof{
--    logMsg("resolve update at $(Lc) of $(Rc).$(Fld)");
    RcTp = typeOf(Rc);
    if AccFn ?= findUpdate(Lc,RcTp,Fld,Dict) then{
--      logMsg("updater fun $(AccFn)\:$(typeOf(AccFn))");

      Ft = newTypeVar("F");
      if sameType(typeOf(AccFn),funType([RcTp,Ft],RcTp),Dict) then{
	if sameType(typeOf(Vl),snd(freshen(Ft,Dict)),Dict) then{
--	  logMsg("update $(update(Lc,Rc,Fld,Vl)) resolved to $(apply(Lc,AccFn,[Rc,Vl],RcTp))");
	  valis overloadTerm(apply(Lc,AccFn,[Rc,Vl],RcTp),Dict,markResolved(St))
	} else{
	  valis (update(Lc,Rc,Fld,Vl),
	    .active(Lc,"field $(Fld)\:$(Ft) not consistent with required type $(typeOf(Vl))"))
	}
      }
      else {
	valis (update(Lc,Rc,Fld,Vl),
	  .active(Lc,"updater for field $(Fld) for $(RcTp) not consistent with required type $(typeOf(Vl))"))
      }
    } else {
      valis (update(Lc,Rc,Fld,Vl),
	.active(Lc,"cannot find updater for field $(Fld) for $(RcTp) not consistent with required type $(typeOf(Vl))"))
    }
  }

  resolveState ::= .inactive |
    .resolved |
    .active(option[locn],string).

  markResolved(.inactive) => .resolved.
  markResolved(St) => St.

  implementation display[resolveState] => {
    disp(.inactive) => "inactive".
    disp(.resolved) => "resolved".
    disp(.active(Lc,Msg)) => "active $(Lc)\:#(Msg)".
  }

  public lambdaLbl(Lc) => genSym((ALc?=Lc?locPkg(ALc)||"")++"λ").
}
  
