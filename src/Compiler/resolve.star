star.compiler.resolve{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.terms.
  import star.compiler.types.
  import star.compiler.freshen.
  import star.compiler.unify.

  public overloadEnvironment:(cons[cons[canonDef]],dict,reports) =>
    either[reports,cons[cons[canonDef]]].
  overloadEnvironment(Gps,Dict,Rp) => do{
    logMsg("resolving definitions in $(Gps)");
    TDict .= declareImplementations(Gps,Dict);
--    logMsg("resolution dict = $(TDict)");
    overloadGroups(Gps,[],TDict,Rp)
  }

  declareImplementations([],Dict) => Dict.
  declareImplementations([Gp,..Gps],Dict) =>
    declareImplementations(Gps,declareImplementationsInGroup(Gp,Dict)).

  declareImplementationsInGroup:(cons[canonDef],dict) => dict.
  declareImplementationsInGroup([],Dict) => Dict.
  declareImplementationsInGroup([implDef(Lc,_,FullNm,_,_,Tp),..Gp],Dict) =>
    declareImplementationsInGroup(Gp,
      declareVr(FullNm,some(Lc),Tp,(LL,TT)=>vr(Lc,FullNm,Tp),
	declareImplementation(FullNm,Tp,Dict))).
  declareImplementationsInGroup([_,..Gp],Dict) => declareImplementationsInGroup(Gp,Dict).

  overloadGroups:(cons[cons[canonDef]],cons[cons[canonDef]],dict,reports) =>
    either[reports,cons[cons[canonDef]]].
  overloadGroups([],Gps,_,_) => either(reverse(Gps)).
  overloadGroups([Gp,..Gps],RG,Dict,Rp) => do{
--    logMsg("overload group $(Gp)");
--    logMsg("dict is $(Dict)");
    (RGp,GDict) <- overloadGroup(Gp,Dict,Rp);
    overloadGroups(Gps,[RGp,..RG],GDict,Rp)
  }

  public overloadGroup:(cons[canonDef],dict,reports)=>either[reports,(cons[canonDef],dict)].
  overloadGroup(Dfs,Dict,Rp) => do{
    RDefs <- overloadDefs(Dict,Dfs,[],Rp);
    TDict .= declareImplementationsInGroup(Dfs,Dict);
    valis (RDefs,TDict)
  }

  overloadDefs:(dict,cons[canonDef],cons[canonDef],reports) =>
    either[reports,cons[canonDef]].
  overloadDefs(Dict,[],Dfx,Rp) => either(reverse(Dfx)).
  overloadDefs(Dict,[D,..Defs],Dfx,Rp) => do{
    DD <- overloadDef(Dict,D,Rp);
    overloadDefs(Dict,Defs,[DD,..Dfx],Rp)
  }

  overloadDef:(dict,canonDef,reports)=>either[reports,canonDef].
  overloadDef(Dict,varDef(Lc,Nm,FullNm,Val,Cx,Tp),Rp) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,Val,[CTp | typeConstraint(CTp) in Cx],Tp,Rp). 
  overloadDef(Dict,implDef(Lc,Nm,FullNm,Val,Cx,Tp),Rp) =>
    overloadImplDef(Dict,Lc,Nm,FullNm,Val,[CTp | typeConstraint(CTp) in Cx],Tp,Rp).
  overloadDef(Dict,Def,Rp) default => either(Def).
 
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,[],Tp,Rp) => do{
    RVal <- resolveTerm(Val,Dict,Rp);
    valis varDef(Lc,Nm,FullNm,RVal,[],Tp)
  }
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp) => do{
    (Cvrs,CDict) .= defineCVars(Lc,Cx,[],Dict);
    RVal <- resolveTerm(Val,CDict,Rp);
    (Qx,Qt) .= deQuant(Tp);
    (_,ITp) .= deConstrain(Qt);
    CTp .= reQuant(Qx,funType(Cx,ITp));
    valis varDef(Lc,Nm,FullNm,lambda([eqn(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp)
  }

  overloadImplDef(Dict,Lc,Nm,FullNm,Val,[],Tp,Rp) => do{
    IDict .= undeclareVar(FullNm,Dict);
    RVal <- resolveTerm(Val,IDict,Rp);
    valis implDef(Lc,Nm,FullNm,RVal,[],Tp)
  }
  overloadImplDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp) => do{
--    logMsg("overloading implementation $(Nm)\:$(Val)");
    (Cvrs,CDict) .= defineCVars(Lc,Cx,[],Dict);
    RVal <- resolveTerm(Val,CDict,Rp);
    (Qx,Qt) .= deQuant(Tp);
    (_,ITp) .= deConstrain(Qt);
    CTp .= reQuant(Qx,funType(Cx,ITp));
    valis implDef(Lc,Nm,FullNm,lambda([eqn(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp)
  }

  defineCVars:(locn,cons[tipe],cons[canon],dict) => (cons[canon],dict).
  defineCVars(_,[],Vrs,D) => (reverse(Vrs),D).
  defineCVars(Lc,[T,..Tps],Vrs,D) where TpNm .= implementationName(T) =>
    defineCVars(Lc,Tps,[vr(Lc,TpNm,T),..Vrs],declareVar(TpNm,TpNm,some(Lc),T,D)).

  resolveState ::= .inactive | .resolved | active(locn,string).

  markResolved(.inactive) => .resolved.
  markResolved(St) default => St.

  public resolveTerm(Term,Dict,Rp) => do{
    (St,RTerm) <- overloadTerm(Term,Dict,.inactive,Rp);
    resolveAgain(.inactive,St,RTerm,Dict,Rp)
  }

  resolveAgain(_,.resolved,Term,Dict,Rp) => resolveTerm(Term,Dict,Rp).
  resolveAgain(_,.inactive,Term,_,_) => either(Term).
  resolveAgain(active(_,_),active(Lc,Msg),_,_,Rp) =>
    other(reportError(Rp,Msg,Lc)).
  resolveAgain(_,active(Lc,Msg),Term,Dict,Rp) => do{
    (St,RTerm) <- overloadTerm(Term,Dict,.inactive,Rp);
    resolveAgain(active(Lc,Msg),St,RTerm,Dict,Rp)
  }

  overloadTerm:(canon,dict,resolveState,reports) => either[reports,(resolveState,canon)].
  overloadTerm(vr(Lc,Nm,Tp),_,St,Rp) => either((St,vr(Lc,Nm,Tp))).
  overloadTerm(intr(Lc,Ix),_,St,Rp) => either((St,intr(Lc,Ix))).
  overloadTerm(flt(Lc,Dx),_,St,Rp) => either((St,flt(Lc,Dx))).
  overloadTerm(strng(Lc,Sx),_,St,Rp) => either((St,strng(Lc,Sx))).
  overloadTerm(enm(Lc,FullNm,Tp),_,St,Rp) => either((St,enm(Lc,FullNm,Tp))).
  overloadTerm(dot(Lc,Rc,Fld,Tp),Dict,St,Rp) => do{
    (Stx,Rc1) <- overloadTerm(Rc,Dict,St,Rp);
    valis (Stx,dot(Lc,Rc1,Fld,Tp))
  }
  overloadTerm(whr(Lc,T,C),Dict,St,Rp) => do{
    (St1,OT) <- overloadTerm(T,Dict,St,Rp);
    (Stx,OC) <- overloadTerm(C,Dict,St1,Rp);
    valis (Stx,whr(Lc,OT,OC))
  }
  overloadTerm(over(Lc,T,Tp,Cx),Dict,St,Rp) => do{
--    logMsg("overloading $(T)\:$(Tp)");
    (St1,[A,..Args]) <- resolveContracts(Lc,Cx,[],Dict,St,Rp);
    if mtd(_,Nm,MTp) .= T then{
      if _eof(Args) then
	valis (St1,dot(Lc,A,Nm,Tp))
      else
      valis (St1,apply(Lc,dot(Lc,A,Nm,MTp),tple(Lc,Args),Tp))
    }
    else
    valis (St1,apply(Lc,T,tple(Lc,[A,..Args]),Tp))
  }
  overloadTerm(apply(lc,Op,Arg,Tp),Dict,St,Rp) => do{
    (St1,ROp) <- overloadTerm(Op,Dict,St,Rp);
    (St2,RArgs) <- overloadTerm(Arg,Dict,St,Rp);
    valis (St2,apply(lc,ROp,RArgs,Tp))
  }
  overloadTerm(tple(Lc,Els),Dict,St,Rp) => do{
    (Stx,REls) <- overloadTerms(Els,[],Dict,St,Rp);
    valis (Stx,tple(Lc,REls))
  }
  overloadTerm(abstraction(Lc,Exp,Cond,Tp),Dict,St,Rp) => do{
    (St1,RExp) <- overloadTerm(Exp,Dict,St,Rp);
    (St2,RCond) <- overloadTerm(Cond,Dict,St1,Rp);
    valis (St2,abstraction(Lc,RExp,RCond,Tp))
  }
  overloadTerm(serch(Lc,Ptn,Src,Iter),Dict,St,Rp) => do{
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St,Rp);
    (St2,RSrc) <- overloadTerm(Src,Dict,St1,Rp);
    (St3,RIter) <- overloadTerm(Iter,Dict,St2,Rp);
    valis (St3,serch(Lc,RPtn,RSrc,RIter))
  }
  overloadTerm(match(Lc,Ptn,Src),Dict,St,Rp) => do{
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St,Rp);
    (St2,RSrc) <- overloadTerm(Src,Dict,St1,Rp);
    valis (St2,match(Lc,RPtn,RSrc))
  }
  overloadTerm(conj(Lc,Lhs,Rhs),Dict,St,Rp) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St,Rp);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1,Rp);
    valis (St2,conj(Lc,RLhs,RRhs))
  }
  overloadTerm(disj(Lc,Lhs,Rhs),Dict,St,Rp) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St,Rp);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1,Rp);
    valis (St2,disj(Lc,RLhs,RRhs))
  }
  overloadTerm(neg(Lc,Rhs),Dict,St,Rp) => do{
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St,Rp);
    valis (St2,neg(Lc,RRhs))
  }
  overloadTerm(cond(Lc,Tst,Lhs,Rhs),Dict,St,Rp) => do{
    (St1,RTst) <- overloadTerm(Tst,Dict,St,Rp);
    (St2,RLhs) <- overloadTerm(Lhs,Dict,St1,Rp);
    (St3,RRhs) <- overloadTerm(Rhs,Dict,St2,Rp);
    valis (St3,cond(Lc,RTst,RLhs,RRhs))
  }
  overloadTerm(lambda(Rls,Tp),Dict,St,Rp) => do{
    (Stx,RRls) <- overloadRules(Rls,[],Dict,St,Rp);
    valis (Stx,lambda(RRls,Tp))
  }
  overloadTerm(letExp(Lc,Gp,Rhs),Dict,St,Rp) => do{
    (RDfs,RDct) <- overloadGroup(Gp,Dict,Rp);
    (St2,RRhs) <- overloadTerm(Rhs,RDct,St,Rp);
    valis (St2,letExp(Lc,RDfs,RRhs))
  }
  overloadTerm(letRec(Lc,Gp,Rhs),Dict,St,Rp) => do{
    (RDfs,RDct) <- overloadGroup(Gp,Dict,Rp);
    (St2,RRhs) <- overloadTerm(Rhs,RDct,St,Rp);
    valis (St2,letRec(Lc,RDfs,RRhs))
  }
  overloadTerm(act(Lc,Act),Dict,St,Rp) => do{
    (St1,RAct) <- overloadAction(Act,Dict,St,Rp);
    valis (St1,act(Lc,RAct))
  }
  overloadTerm(record(Lc,Nm,Fields,Tp),Dict,St,Rp) => do{
    (Stx,RFields) <- overloadFields(Fields,[],Dict,St,Rp);
    valis (St,record(Lc,Nm,RFields,Tp))
  }
  overloadTerm(update(Lc,T,C),Dict,St,Rp) => do{
    (St1,OT) <- overloadTerm(T,Dict,St,Rp);
    (Stx,OC) <- overloadTerm(C,Dict,St1,Rp);
    valis (Stx,update(Lc,OT,OC))
  }

  overloadRules([],Els,Dict,St,_) => either((St,reverse(Els))).
  overloadRules([eqn(Lc,Ptn,.none,Exp),..Ts],Els,Dict,St,Rp) => do{
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St,Rp);
    (St2,RExp) <- overloadTerm(Exp,Dict,St1,Rp);
    overloadRules(Ts,[eqn(Lc,RPtn,.none,RExp),..Els],Dict,St2,Rp)
  }
  overloadRules([eqn(Lc,Ptn,some(Wh),Exp),..Ts],Els,Dict,St,Rp) => do{
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St,Rp);
    (St2,RExp) <- overloadTerm(Exp,Dict,St1,Rp);
    (Stx,RWh) <- overloadTerm(Wh,Dict,St2,Rp);
    overloadRules(Ts,[eqn(Lc,RPtn,some(RWh),RExp),..Els],Dict,Stx,Rp)
  }

  overloadTerms([],Els,Dict,St,_) => either((St,reverse(Els))).
  overloadTerms([T,..Ts],Els,Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    overloadTerms(Ts,[RT,..Els],Dict,St1,Rp)
  }

  overloadFields([],Els,Dict,St,_) => either((St,reverse(Els))).
  overloadFields([(N,T),..Ts],Els,Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    overloadFields(Ts,[(N,RT),..Els],Dict,St1,Rp)
  }

  overloadAction(noDo(Lc),Dict,St,Rp) => either((St,noDo(Lc))).
  overloadAction(seqnDo(Lc,A1,A2),Dict,St,Rp) => do{
    (St1,RA1) <- overloadAction(A1,Dict,St,Rp);
    (St2,RA2) <- overloadAction(A2,Dict,St1,Rp);
    valis (St2,seqnDo(Lc,RA1,RA2))
  }
  overloadAction(bindDo(Lc,Lhs,Rhs,T1,T2,T3),Dict,St,Rp) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St,Rp);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1,Rp);
    valis (St2,bindDo(Lc,RLhs,RRhs,T1,T2,T3))
  }
  overloadAction(varDo(Lc,Lhs,Rhs),Dict,St,Rp) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St,Rp);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1,Rp);
    valis (St2,varDo(Lc,RLhs,RRhs))
  }
  overloadAction(delayDo(Lc,A1,T1,T2,T3),Dict,St,Rp) => do{
    (St1,RA1) <- overloadAction(A1,Dict,St,Rp);
    valis (St1,delayDo(Lc,RA1,T1,T2,T3))
  }
  overloadAction(whileDo(Lc,T,A,T1,T2),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    (St2,RA) <- overloadAction(A,Dict,St1,Rp);
    valis (St2,whileDo(Lc,RT,RA,T1,T2))
  }
  overloadAction(forDo(Lc,T,A,T1,T2),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    (St2,RA) <- overloadAction(A,Dict,St1,Rp);
    valis (St2,forDo(Lc,RT,RA,T1,T2))
  }
  overloadAction(ifThenElseDo(Lc,T,L,R,T1,T2,T3),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    (St2,RL) <- overloadAction(L,Dict,St1,Rp);
    (St3,RR) <- overloadAction(R,Dict,St2,Rp);
    valis (St3,ifThenElseDo(Lc,RT,RL,RR,T1,T2,T3))
  }
  overloadAction(tryCatchDo(Lc,A,C,T1,T2,T3),Dict,St,Rp) => do{
    (St1,RA) <- overloadAction(A,Dict,St,Rp);
    (St2,RC) <- overloadTerm(C,Dict,St1,Rp);
    valis (St2,tryCatchDo(Lc,RA,RC,T1,T2,T3))
  }
  overloadAction(throwDo(Lc,T,T1,T2,T3),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    valis (St1,throwDo(Lc,RT,T1,T2,T3))
  }
  overloadAction(returnDo(Lc,T,T1,T2,T3),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    valis (St1,returnDo(Lc,RT,T1,T2,T3))
  }
  overloadAction(simpleDo(Lc,T,T1),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    valis (St1,simpleDo(Lc,RT,T1))
  }
    
  resolveContracts:(locn,cons[constraint],cons[canon],dict,resolveState,reports) =>
    either[reports,(resolveState,cons[canon])].
  resolveContracts(_,[],Cx,_,St,Rp) => either((St,reverse(Cx))).
  resolveContracts(Lc,[typeConstraint(C),..Cx],Vs,Dict,St,Rp) => do{
    (St0,A) <- resolveContract(Lc,C,Dict,St,Rp);
    resolveContracts(Lc,Cx,[A,..Vs],Dict,St0,Rp)
  }
  
  resolveContract:(locn,tipe,dict,resolveState,reports) => either[reports,(resolveState,canon)].
  resolveContract(Lc,Tp,Dict,St,Rp) => do{
    ImpNm .= implementationName(Tp);
--    logMsg("looking for implementation $(Tp) - $(ImpNm)");
    if vrEntry(_,Mk,VTp)^=isVar(ImpNm,Dict) then {
--      logMsg("we have implementation $(Mk(Lc,Tp)) for $(VTp)");
      (_,VrTp) .= freshen(VTp,Dict);
      
      (ITp,Impl) <- manageConstraints(VrTp,[],Lc,Mk(Lc,Tp),Dict,Rp);
--      logMsg("deconstrained implementation $(ITp)");
      if sameType(ITp,Tp,Dict) then {
--	logMsg("we found implementation $(Impl)\:$(ITp)");
	overloadTerm(Impl,Dict,markResolved(St),Rp)
      } else{
	throw reportError(Rp,"implementation $(ITp) not consistent with $(Tp)",Lc)
      }
    } else{
      throw reportError(Rp,"cannot find an implementation for $(Tp)",Lc)
    }
  }
}
  
