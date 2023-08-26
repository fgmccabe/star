star.compiler.resolve{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.freevars.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.term.
  import star.compiler.types.
  import star.compiler.unify.

  public overloadProgram:(cons[cons[canonDef]],dict) => cons[cons[canonDef]].
  overloadProgram(Gps,Dict) => overloadGroups(Gps,[],Dict).

  contract all e ~~ resolve[e] ::= {
    resolve:(e,dict,resolveState) => (e,resolveState)
  }

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
    (DD,DDict) = overloadDef(D,Dict);
    valis overloadDefs(Defs,DDict,[DD,..Dfx])
  }

  overloadDef:(canonDef,dict)=>(canonDef,dict).
  overloadDef(.varDef(Lc,Nm,.lambda(_,_,Eqns,_,_),Cx,Tp),Dict) =>
    overloadFunction(Dict,Lc,Nm,Eqns,Cx,Tp).
  overloadDef(.varDef(Lc,Nm,Val,Cx,Tp),Dict) =>
    overloadVarDef(Dict,Lc,Nm,Val,Cx,Tp).
  overloadDef(.implDef(Lc,Nm,FullNm,Val,Cx,Tp),Dict) =>
    overloadImplDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp).
  overloadDef(.typeDef(Lc,Nm,Tp,TpRl),Dict) => (.typeDef(Lc,Nm,Tp,TpRl),Dict).
  overloadDef(.cnsDef(Lc,Nm,Ix,Tp),Dict) => (.cnsDef(Lc,Nm,Ix,Tp),Dict).
  overloadDef(Def,Dict) default => valof{
    reportError("cannot overload $(Def)",locOf(Def));
    valis (Def,Dict)
  }

  overloadFunction:(dict,option[locn],string,cons[rule[canon]],cons[constraint],tipe)=>
    (canonDef,dict).
  overloadFunction(Dict,Lc,Nm,Eqns,Cx,Tp) => valof{
    if traceCanon! then
      logMsg("overload function $(Nm) = $(Eqns), Cx=$(Cx)");
    (Extra,CDict) = defineCVars(Lc,Cx,[],Dict);
      
    REqns = Eqns//(Eq)=>resolveEqn(Eq,Extra,CDict);
    (Qx,Qt) = deQuant(Tp);
    (_,ITp) = deConstrain(Qt);
    if .tupleType(AITp).=funTypeArg(ITp) && RITp .= funTypeRes(ITp) then {
      CTp = reQuant(Qx,funType((Cx//typeOf)++AITp,RITp));
      if traceCanon! then
	logMsg("overloaded fun $(.varDef(Lc,Nm,.lambda(Lc,Nm,REqns,[],CTp),[],CTp))");
      valis (.varDef(Lc,Nm,.lambda(Lc,Nm,REqns,[],CTp),[],CTp),Dict)
    } else{
      reportError("type of $(Nm) not a function type",Lc);
      valis (.varDef(Lc,Nm,.lambda(Lc,Nm,REqns,[],Tp),[],Tp),Dict)
    }
  }

  resolveEqn(Rl,Extra,Dict) => valof{
    if .rule(Lc,A,C,V) .= overload(Rl,Dict) then{
      valis .rule(Lc,addExtra(A,Extra),C,V)
    } else{
      reportError("not a rule: $(Rl)",locOf(Rl));
      valis Rl
    }
  }
 
  overloadVarDef:(dict,option[locn],string,canon,cons[constraint],tipe)=>
    (canonDef,dict).
  overloadVarDef(Dict,Lc,Nm,Val,[],Tp) => 
    (.varDef(Lc,Nm,overload(Val,Dict),[],Tp),Dict).
  overloadVarDef(Dict,Lc,Nm,Val,Cx,Tp) => valof{
    (Cvrs,CDict) = defineCVars(Lc,Cx,[],Dict);
    RVal = overload(Val,CDict);
    (Qx,Qt) = deQuant(Tp);
    (_,ITp) = deConstrain(Qt);
    CTp = reQuant(Qx,funType(Cx//typeOf,ITp));
    valis (.varDef(Lc,Nm,.lambda(Lc,lambdaLbl(Lc),[.rule(Lc,.tple(Lc,Cvrs),.none,RVal)],[],CTp),[],Tp),Dict)
  }

  overloadImplDef:(dict,option[locn],string,string,canon,cons[constraint],tipe) =>
    (canonDef,dict).
  overloadImplDef(Dict,Lc,Nm,FullNm,Val,_,Tp) => valof{
    (Qx,Qt) = deQuant(Tp);
    (Cx,ITp) = deConstrain(Qt);

    (Cvrs,CDict) = defineCVars(Lc,Cx,[],Dict);

    RVal = overload(Val,CDict);

    if isEmpty(Cvrs) then {
      CTp = reQuant(Qx,ITp);
      valis (.implDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
    } else {
      CTp = reQuant(Qx,funType(Cx//genContractType,ITp));
      valis (.implDef(Lc,Nm,FullNm,.lambda(Lc,lambdaLbl(Lc),[.rule(Lc,.tple(Lc,Cvrs),.none,RVal)],[],CTp),[],Tp),Dict)
    }
  }

  genContractType(.conTract(Nm,Tps,Dps)) => mkConType(Nm,Tps,Dps).
  genContractType(.implicit(Nm,Tp)) => Tp.

  defineCVars:(option[locn],cons[constraint],cons[canon],dict) => (cons[canon],dict).
  defineCVars(_,[],Vrs,D) => (reverse(Vrs),D).
  defineCVars(Lc,[(T where .conTract(CNm,CTps,CDTps).=T),..Tps],Vrs,D) => valof{
    TpNm = implementationName(T);
    Tp=typeOf(T);
    valis defineCVars(Lc,Tps,[.vr(Lc,TpNm,Tp),..Vrs],
      declareVar(TpNm,TpNm,Lc,Tp,.none,
	declareImplementation(Lc,TpNm,TpNm,Tp,D)))
  }
  defineCVars(Lc,[.hasField(Tp,Nm,FTp),..Tps],Vrs,D) => valof{
    Vnm = genId("Nm");
    Vtp = funType([Tp],FTp);
    valis defineCVars(Lc,Tps,[.vr(Lc,Vnm,Vtp),..Vrs],
      declareVar(Vnm,Vnm,Lc,Vtp,.none,
	declareAccessor(Lc,Tp,Nm,Vnm,Vtp,D)))
  }
  defineCVars(Lc,[.implicit(Nm,Tp),..Tps],Vrs,D) => valof{
    valis defineCVars(Lc,Tps,[.vr(Lc,Nm,Tp),..Vrs],
      declareVar(Nm,Nm,Lc,Tp,.none,D))
  }
  defineCVars(Lc,[.raisEs(Tp),..Tps],Vrs,D) => valof{
    QNm = qualifiedName("$R",.tractMark,tpName(deRef(Tp)));
    if traceCanon! then
      logMsg("defining try scope for $(Tp) with #(QNm)");
    valis defineCVars(Lc,Tps,[.vr(Lc,QNm,Tp),..Vrs],
      declareTryScope(Lc,Tp,QNm,D))
  }

  defineArgVars(Ptn,D) =>
    foldLeft(defineArg,D,ptnVars(Ptn,[],[])).

  defineArg(.cId(Nm,Tp),D) => declareVar(Nm,Nm,.none,Tp,.none,D).

  overload:all e ~~ resolve[e] |: (e,dict) => e.
  overload(C,D) => resolveAgain(.inactive,C,resolve(C,D,.inactive),D).

  resolveAgain:all e ~~ resolve[e] |: (resolveState,e,(e,resolveState),dict) => e.
  resolveAgain(_,_,(T,.resolved),D) =>
    resolveAgain(.inactive,T,resolve(T,D,.inactive),D).
  resolveAgain(_,_,(T,.inactive),_) => T.
  resolveAgain(.active(_,Msg),_,(T,.active(Lc,Msg)),_) => valof{
    reportError(Msg,Lc);
    valis T
  }
  resolveAgain(_,O,(_,.active(Lc,Msg)),D) =>
    resolveAgain(.active(Lc,Msg),O,resolve(O,D,.inactive),D).

  implementation resolve[canon] => {
    resolve(T,D,S) => overloadTerm(T,D,S)
  }

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
  overloadTerm(.tdot(Lc,Rc,Fld,Tp),Dict,St) => valof{
    (Rc1,St1) = overloadTerm(Rc,Dict,St);
    valis resolveTDot(Lc,Rc1,Fld,Tp,Dict,St1);
  }
  overloadTerm(.tple(Lc,Els),Dict,St) => valof{
    (REls,St1) = overloadTplEls(Els,Dict,St);
    valis (.tple(Lc,REls),St1)
  }
  overloadTerm(.owpen(Lc,Rc),Dict,St) => valof{
    (Rc1,St1) = overloadTerm(Rc,Dict,St);
    valis (.owpen(Lc,Rc1),St1);
  }
  overloadTerm(.mtd(Lc,Nm,Tp),Dict,St) => 
    (.mtd(Lc,Nm,Tp),.active(Lc,"cannot resolve unconstrained method #(Nm)\:$(Tp)")).
  overloadTerm(.over(Lc,.rais(Lc,_,E,ETp),Cx),Dict,St) => valof{
    (Trw,St1) = resolveConstraint(Lc,Cx,Dict,St);
    (ErE,St2) = overloadTerm(E,Dict,St1);
    valis (.rais(Lc,Trw,ErE,ETp),markResolved(St2))
  }
  overloadTerm(.over(Lc,T,Cx),Dict,St) => valof{
    (DArg,St1) = resolveConstraint(Lc,Cx,Dict,St);
    (OverOp,NArgs,St2) = resolveRef(T,DArg,[],Dict,St1);
    valis (overApply(Lc,OverOp,NArgs,typeOf(T)),markResolved(St2))
  }
  overloadTerm(.apply(lc,.over(OLc,T,Cx),Args,Tp),Dict,St) => valof{
    (DArg,St1) = resolveConstraint(OLc,Cx,Dict,St);
    (RArgs,St2) = overloadTplEls(Args,Dict,St1);
    (OverOp,NArgs,St3) = resolveRef(T,DArg,RArgs,Dict,St2);
    valis (.apply(lc,OverOp,NArgs,Tp),markResolved(St3))
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
  overloadTerm(.lambda(Lc,Nm,Rls,Cx,Tp),Dict,St) => valof{
    if traceCanon! then
      logMsg("overload lambda $(.lambda(Lc,Nm,Rls,Cx,Tp))");
    
    (Extra,CDict) = defineCVars(Lc,Cx,[],Dict);
    (RRls,St1) = overloadRules(Extra,Rls,CDict,St);

    if traceCanon! then
      logMsg("overloaded lambda $(.lambda(Lc,Nm,RRls,[],Tp))\:$(Tp)");
    
    valis (.lambda(Lc,Nm,RRls,[],Tp),St1)
  }
  overloadTerm(.letExp(Lc,Gp,Decls,Rhs),Dict,St) => valof{
    TDict = declareDecls(Decls,Dict);
    (RDfs,_) = overloadGroup(Gp,TDict);
    (RRhs,St1) = overloadTerm(Rhs,TDict,St);
    valis (.letExp(Lc,RDfs,Decls,RRhs),St1)
  }
  overloadTerm(.letRec(Lc,Gp,Decs,Rhs),Dict,St) => valof{
    LDict = declareDecls(Decs,Dict);
    (RDfs,RDct) = overloadGroup(Gp,LDict);
    (RRhs,St2) = overloadTerm(Rhs,RDct,St);
    valis (.letRec(Lc,RDfs,Decs,RRhs),St2)
  }
  overloadTerm(.csexp(Lc,Gov,Cases,Tp),Dict,St) => valof{
    (RGov,St1) = overloadTerm(Gov,Dict,St);
    (RCases,St2) = overloadRules([],Cases,Dict,St1);
    valis (.csexp(Lc,RGov,RCases,Tp),St2)
  }
  overloadTerm(.trycatch(Lc,B,.vr(TLc,Tnm,ErTp),Hs,Tp),Dict,St) => valof{
    TDict = declareTryScope(Lc,ErTp,Tnm,Dict);
    (BB,St1) = overloadTerm(B,TDict,St);
    (HH,St2) = overloadRules([],Hs,Dict,St1);
    valis (.trycatch(Lc,BB,.vr(TLc,Tnm,ErTp),HH,Tp),St2)
  }
  overloadTerm(.rais(Lc,T,E,Tp),Dict,St) => valof{
    (TT,St1) = overloadTerm(T,Dict,St);
    (EE,St2) = overloadTerm(E,Dict,St1);
    valis (.rais(Lc,TT,EE,Tp),St2)
  }
  overloadTerm(.spwn(Lc,Lm,Tp),Dict,St) => valof{
    (RLm,St1) = overloadTerm(Lm,Dict,St);
    valis (.spwn(Lc,RLm,Tp),St1)
  }
  overloadTerm(.paus(Lc,Lm,Tp),Dict,St) => valof{
    (RLm,St1) = overloadTerm(Lm,Dict,St);
    valis (.paus(Lc,RLm,Tp),St1)
  }
  overloadTerm(.susp(Lc,Fb,Ex,Tp),Dict,St) => valof{
    (RFb,St1) = overloadTerm(Fb,Dict,St);
    (REx,St2) = overloadTerm(Ex,Dict,St1);
    valis (.susp(Lc,RFb,REx,Tp),St2)
  }
  overloadTerm(.rsme(Lc,Fb,Ex,Tp),Dict,St) => valof{
    (RFb,St1) = overloadTerm(Fb,Dict,St);
    (REx,St2) = overloadTerm(Ex,Dict,St1);
    valis (.rsme(Lc,RFb,REx,Tp),St2)
  }
  overloadTerm(.rtire(Lc,Fb,Ex),Dict,St) => valof{
    (RFb,St1) = overloadTerm(Fb,Dict,St);
    (REx,St2) = overloadTerm(Ex,Dict,St1);
    valis (.rtire(Lc,RFb,REx),St2)
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
  
  resolveRef(.mtd(Lc,Nm,Tp),DT,Args,Dict,St) => valof{
    (OverOp,St1) = resolveDot(Lc,DT,Nm,Tp,Dict,St);
    valis (OverOp,Args,St1)
  }
  resolveRef(C,DArg,Args,_,St) default =>
    (C,[DArg,..Args],St).

  overApply(_,OverOp,[],_) => OverOp.
  overApply(Lc,OverOp,Args,Tp) where ~ _ ?= isFunType(Tp) =>
    .apply(Lc,OverOp,Args,Tp).
  overApply(Lc,OverOp,Args,Tp) =>
    curryOver(Lc,OverOp,Args,Tp).

  curryOver(Lc,OverOp,Args,Tp) where .tupleType(ArgTps) .= funTypeArg(Tp) => valof{
    Vrs = { .vr(Lc,genSym("A"),ArgTp) | ArgTp in ArgTps};
    NArgs = Args++Vrs;
    valis .lambda(Lc,lambdaLbl(Lc),
      [.rule(Lc,.tple(Lc,Vrs),.none,.apply(Lc,OverOp,NArgs,Tp))],
      [],
      funType(ArgTps,Tp))
  }

  implementation resolve[canonAction] => {
    resolve(T,D,S) => overloadAction(T,D,S)
  }

  overloadAction(.doNop(Lc),_,St) => (.doNop(Lc),St).
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
  overloadAction(.doRetire(Lc,T,E),Dict,St) => valof{
    (TT,St1) = overloadTerm(T,Dict,St);
    (EE,St2) = overloadTerm(E,Dict,St1);
    valis (.doRetire(Lc,TT,EE),St1)
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
  overloadAction(.doTryCatch(Lc,A,.vr(TLc,Tnm,ErTp),H),Dict,St) => valof{
    TDict = declareTryScope(Lc,ErTp,Tnm,Dict);
    (AA,St1) = overloadAction(A,TDict,St);
    (HH,St2) = overloadRules([],H,Dict,St1);
    valis (.doTryCatch(Lc,AA,.vr(TLc,Tnm,ErTp),HH),St2)
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
    TDict = declareDecls(Decls,Dict);
    (RDfs,_) = overloadGroup(Gp,TDict);
    (BB,St1) = overloadAction(B,TDict,St);
    valis (.doLet(Lc,RDfs,Decls,BB),St1)
  }
  overloadAction(.doLetRec(Lc,Gp,Decs,B),Dict,St) => valof{
    (RDfs,RDct) = overloadGroup(Gp,declareDecls(Decs,Dict));
    (BB,St2) = overloadAction(B,RDct,St);
    valis (.doLetRec(Lc,RDfs,Decs,BB),St2)
  }
  overloadAction(.doCase(Lc,A,H),Dict,St) => valof{
    (AA,St1) = overloadTerm(A,Dict,St);
    (HH,St2) = overloadRules([],H,Dict,St1);
    valis (.doCase(Lc,AA,HH),St2)
  }
  overloadAction(.doExp(Lc,E),Dict,St) => valof{
    (EE,St1) = overloadTerm(E,Dict,St);
    
    valis (.doExp(Lc,EE),St1)
  }
    
  implementation all e ~~ resolve[e] |: resolve[rule[e]] => {
    resolve(T,D,S) => overloadRule([],T,D,S)
  }

  overloadRule:all e ~~ resolve[e] |: (cons[canon],rule[e],dict,resolveState) =>
    (rule[e],resolveState).
  overloadRule(Extra,.rule(Lc,Ptn,.none,Exp),Dict,St) => valof{
    RDict = defineArgVars(Ptn,Dict);
    (RPtn,St1) = overloadTerm(Ptn,RDict,St);
    (RExp,St2) = resolve(Exp,RDict,St1);
    valis (.rule(Lc,addExtra(RPtn,Extra),.none,RExp),St2)
  }
  overloadRule(Extra,.rule(Lc,Ptn,.some(C),Exp),Dict,St) => valof{
    RDict = defineArgVars(Ptn,Dict);
    (RPtn,St1) = overloadTerm(Ptn,RDict,St);
    (RExp,St2) = resolve(Exp,RDict,St1);
    (RC,St3) = resolve(C,RDict,St2);
    valis (.rule(Lc,addExtra(RPtn,Extra),.some(RC),RExp),St3)
  }

  addExtra(.tple(Lc,Els),Extra) => .tple(Lc,Extra++Els).
  
  overloadRules:all x ~~ resolve[x] |: (cons[canon],cons[rule[x]],dict,resolveState)=>
    (cons[rule[x]],resolveState).
  overloadRules(_,[],Dict,St) => ([],St).
  overloadRules(Extra,[Rl,..Rls],Dict,St) => valof{
    (ORl,OSt) = overloadRule(Extra,Rl,Dict,St);
    (ORls,Stx) = overloadRules(Extra,Rls,Dict,OSt);
    valis ([ORl,..ORls],Stx)
  }

  overloadTerms:all e ~~ resolve[e] |: (cons[e],cons[e],dict,resolveState) => (cons[e],resolveState).
  overloadTerms([],Els,Dict,St) => (reverse(Els),St).
  overloadTerms([T,..Ts],Els,Dict,St) => valof{
    (RT,St1) = resolve(T,Dict,St);
    valis overloadTerms(Ts,[RT,..Els],Dict,St1)
  }

  overloadTplEls:all e ~~ resolve[e] |: (cons[e],dict,resolveState) => (cons[e],resolveState).
  overloadTplEls(Els,Dict,St) => overloadTerms(Els,[],Dict,St).
    
  resolveConstraint:(option[locn],constraint,dict,resolveState) => (canon,resolveState).
  resolveConstraint(Lc,.implicit(Id,Tp),Dict,St) => valof{
    if Var ?= findVar(Lc,Id,Dict) then{
      if sameType(snd(freshen(Tp,Dict)),typeOf(Var),Dict) then {
	valis (Var,markResolved(St))
      } else{
	valis (.anon(Lc,Tp),
	  .active(Lc,"implicit $(Id)\:$(typeOf(Var)) not consistent with expected type: $(Tp)"))
      }
    }
    else{
      valis (.anon(Lc,Tp),.active(Lc,"cannot find an definition for implicit var #(Id)\:$(Tp)"))
    }
  }
  resolveConstraint(Lc,.raisEs(Tp),Dict,St) => valof{
    if .varDec(_,_,TrNm,ETp) ?= findTryScope(Tp,Dict) then{
      if sameType(snd(freshen(ETp,Dict)),Tp,Dict) then {
	valis (.vr(Lc,TrNm,Tp),markResolved(St))
      } else{
	valis (.anon(Lc,Tp),
	  .active(Lc,"raises $(ETp) not consistent with expected type: $(Tp)"))
      }
    }
    else{
      valis (.anon(Lc,Tp),.active(Lc,"cannot find a exception context for $(Tp)"))
    }
  }
  
  resolveConstraint(Lc,.hasField(RcTp,Fld,FldTp),Dict,St) => valof{
    if (AccessOp,St1) ?= resolveAccess(Lc,RcTp,Fld,FldTp,Dict,St) then{
      valis (AccessOp,St1)
    } else{
      valis (.anon(Lc,FldTp),.active(Lc,"cannot find accessor for $(RcTp).#(Fld)"))
    }
  }
  resolveConstraint(Lc,Con,Dict,St) => valof{
    ImpNm = implementationName(Con);
    Tp = typeOf(Con);
    if Impl?=findImplementation(Dict,ImpNm) then {
      if sameType(typeOf(Impl),Tp,Dict) then {
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
    if AccFn ?= findAccess(Lc,RcTp,Fld,Dict) then{
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
    if traceCanon! then
      logMsg("resolve $(Rc).#(Fld)\:$(Tp) @ $(Lc)");
    RcTp = typeOf(Rc);
    if AccFn ?= findAccess(Lc,RcTp,Fld,Dict) then{
      if traceCanon! then
	logMsg("access function $(AccFn)\:$(typeOf(AccFn))");
      
      Ft = newTypeVar("F");
      if sameType(typeOf(AccFn),funType([RcTp],Ft),Dict) then{
	FrFt = snd(freshen(Ft,Dict));
	(Cx,FldT) = deConstrain(FrFt);

	if traceCanon! then
	  logMsg("check field type $(Ft)=$(FldT) against $(Tp)");

	if sameType(Tp,FldT,Dict) then{
	  valis overloadTerm(.apply(Lc,AccFn,[Rc],FldT),Dict,markResolved(St))
	} else {
	  valis (.dot(Lc,Rc,Fld,Tp),
	    .active(Lc,"field $(Rc).$(Fld)\:$(Ft) not consistent with required type $(Tp)")).
	}
      } else {
	valis (.dot(Lc,Rc,Fld,Tp),
	  .active(Lc,"accessor for field $(Rc).$(Fld)\:$(typeOf(AccFn)) for $(RcTp) not consistent with required type $(Tp)"))
      }
    } else{
      if traceCanon! then
	logMsg("cannot find accessor for $(Rc)\:$(typeOf(Rc)).#(Fld) in $(Dict)");
      valis (.dot(Lc,Rc,Fld,Tp),.active(Lc,"cannot find accessor for field $(Fld) for $(RcTp)"))
    }
  }

  resolveUpdate:(option[locn],canon,string,canon,dict,resolveState) => (canon,resolveState).
  resolveUpdate(Lc,Rc,Fld,Vl,Dict,St) => valof{
    RcTp = typeOf(Rc);
    if AccFn ?= findUpdate(Lc,RcTp,Fld,Dict) then{
      Ft = newTypeVar("F");
      if sameType(typeOf(AccFn),funType([RcTp,Ft],RcTp),Dict) then{
	if sameType(typeOf(Vl),snd(freshen(Ft,Dict)),Dict) then{
	  valis overloadTerm(.apply(Lc,AccFn,[Rc,Vl],RcTp),Dict,markResolved(St))
	} else{
	  valis (.update(Lc,Rc,Fld,Vl),
	    .active(Lc,"field $(Fld)\:$(Ft) not consistent with required type $(typeOf(Vl))"))
	}
      }
      else {
	valis (.update(Lc,Rc,Fld,Vl),
	  .active(Lc,"updater for field $(Fld) for $(RcTp) not consistent with required type $(typeOf(Vl))"))
      }
    } else {
      valis (.update(Lc,Rc,Fld,Vl),
	.active(Lc,"cannot find updater for field $(Fld) for $(RcTp) not consistent with required type $(typeOf(Vl))"))
    }
  }

  resolveTDot:(option[locn],canon,integer,tipe,dict,resolveState) => (canon,resolveState).
  resolveTDot(Lc,Rc,Ix,Tp,Dict,St) => valof{
    if .tupleType(Els) .= deRef(typeOf(Rc)) then{
      if ElTp ?= Els[Ix] then{
	if sameType(ElTp,Tp,Dict) then{
	  valis (.tdot(Lc,Rc,Ix,Tp),St)
	} else{
	  valis (.tdot(Lc,Rc,Ix,Tp),
	    .active(Lc,"type of $(Rc).$(Ix)\:$(ElTp) not consistent with required type $(Tp)"))
	}
      }
    } else if .faceType(Els,_) .= deRef(typeOf(Rc)) then{
      if (_,ElTp) ?= Els[Ix] then{
	if sameType(ElTp,Tp,Dict) then{
	  valis (.tdot(Lc,Rc,Ix,Tp),St)
	} else{
	  valis (.tdot(Lc,Rc,Ix,Tp),
	    .active(Lc,"type of $(Rc).$(Ix)\:$(ElTp) not consistent with required type $(Tp)"))
	}
      }
    }
    else{
      valis (.tdot(Lc,Rc,Ix,Tp),
	.active(Lc,"type of $(Rc)\:$(typeOf(Rc)) not known to be a tuple type of length > $(Ix)"))
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
}
