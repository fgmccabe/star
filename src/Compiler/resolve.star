star.compiler.resolve{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.freshen.
  import star.compiler.unify.

  public overloadEnvironment:(list[list[canonDef]],list[canon],dict,reports) =>
    either[reports,(list[list[canonDef]],list[canon])].
  overloadEnvironment(Gps,Ots,Dict,Rp) => do{
    TDict = declareImplementations(Gps,Dict);
    logMsg("resolution dict = $(TDict)");
    RGps <- overloadGroups(Gps,[],TDict,Rp);
    ROts <- pickFailures(Ots//(T)=>resolveTerm(T,TDict,Rp));

    valis (RGps,ROts)
  }

  declareImplementations([],Dict) => Dict.
  declareImplementations([Gp,..Gps],Dict) =>
    declareImplementations(Gps,declareGroup(Gp,Dict)).

  declareGroup:(list[canonDef],dict) => dict.
  declareGroup([],Dict) => Dict.
  declareGroup([implDef(Lc,Nm,FullNm,_,Tp),..Gp],Dict) =>
    declareGroup(Gp,declareImplementation(FullNm,Tp,Dict)).
  declareGroup([_,..Gp],Dict) => declareGroup(Gp,Dict).

  overloadGroups:(list[list[canonDef]],list[list[canonDef]],dict,reports) =>
    either[reports,list[list[canonDef]]].
  overloadGroups([],Gps,_,_) => either(Gps).
  overloadGroups([Gp,..Gps],RG,Dict,Rp) => do{
    RGp <- overloadDefs(Dict,Gp,[],Rp);
    overloadGroups(Gps,[RG..,RGp],Dict,Rp)
  }

  overloadDefs:(dict,list[canonDef],list[canonDef],reports) =>
    either[reports,list[canonDef]].
  overloadDefs(Dict,[],Dfx,Rp) => either(Dfx).
  overloadDefs(Dict,[D,..Defs],Dfx,Rp) => do{
    DD <- overloadDef(Dict,D,Rp);
    overloadDefs(Dict,Defs,[Dfx..,DD],Rp)
  }

  overloadDef:(dict,canonDef,reports)=>either[reports,canonDef].
  overloadDef(Dict,varDef(Lc,Nm,FullNm,Val,Cx,Tp),Rp) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,Val,[CTp | typeConstraint(CTp) in Cx],Tp,Rp). 
  overloadDef(Dict,Def,Rp) default => either(Def).
 
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,[],Tp,Rp) => do{
    RVal <- resolveTerm(Val,Dict,Rp);
    valis varDef(Lc,Nm,FullNm,RVal,[],Tp)
  }
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp) => do{
    (Cvrs,CDict) = defineCVars(Lc,Cx,[],Dict);
    RVal <- resolveTerm(Val,CDict,Rp);
    CTp = funType(tupleType(Cx),Tp);
    valis varDef(Lc,Nm,FullNm,lambda([eqn(Lc,tple(Lc,Cvrs),trueEnum(Lc),RVal)],CTp),[],CTp)
  }

  defineCVars:(locn,list[tipe],list[canon],dict) => (list[canon],dict).
  defineCVars(_,[],Vrs,D) => (Vrs,D).
  defineCVars(Lc,[T,..Tps],Vrs,D) where TpNm .= implementationName(T) =>
    defineCVars(Lc,Tps,[Vrs..,vr(Lc,TpNm,T)],declareVar(TpNm,some(Lc),T,D)).

  resolveState ::= inactive | resolved | active(locn,string).

  markResolved(inactive) => resolved.
  markResolved(St) default => St.

  public resolveTerm(Term,Dict,Rp) => do{
    (St,RTerm) <- overloadTerm(Term,Dict,inactive,Rp);
    resolveAgain(inactive,St,RTerm,Dict,Rp)
  }

  resolveAgain(_,resolved,Term,Dict,Rp) => resolveTerm(Term,Dict,Rp).
  resolveAgain(_,inactive,Term,_,_) => either(Term).
  resolveAgain(active(_,_),active(Lc,Msg),_,_,Rp) =>
    other(reportError(Rp,Msg,Lc)).
  resolveAgain(_,active(Lc,Msg),Term,Dict,Rp) => do{
    (St,RTerm) <- overloadTerm(Term,Dict,inactive,Rp);
    resolveAgain(active(Lc,Msg),St,RTerm,Dict,Rp)
  }

  overloadTerm:(canon,dict,resolveState,reports) => either[reports,(resolveState,canon)].
  overloadTerm(vr(Lc,Nm,Tp),_,St,Rp) => either((St,vr(Lc,Nm,Tp))).
  overloadTerm(intLit(Lc,Ix),_,St,Rp) => either((St,intLit(Lc,Ix))).
  overloadTerm(floatLit(Lc,Dx),_,St,Rp) => either((St,floatLit(Lc,Dx))).
  overloadTerm(stringLit(Lc,Sx),_,St,Rp) => either((St,stringLit(Lc,Sx))).
  overloadTerm(enm(Lc,Nm,Tp),_,St,Rp) => either((St,enm(Lc,Nm,Tp))).
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
    (St1,[A,..Args]) <- resolveContracts(Lc,Cx,[],Dict,St,Rp);
    if mtd(_,Nm,MTp) .= T then{
      if _eof(Args) then
	valis (St1,dot(Lc,A,Nm,MTp))
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
  overloadTerm(ixabstraction(Lc,Ky,Val,Cond,Tp),Dict,St,Rp) => do{
    (St1,RKey) <- overloadTerm(Ky,Dict,St,Rp);
    (St2,RVal) <- overloadTerm(Val,Dict,St1,Rp);
    (St3,RCond) <- overloadTerm(Cond,Dict,St2,Rp);
    valis (St3,ixabstraction(Lc,RKey,RVal,RCond,Tp))
  }
  overloadTerm(serch(Lc,Ptn,Src,Cond),Dict,St,Rp) => do{
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St,Rp);
    (St2,RSrc) <- overloadTerm(Src,Dict,St1,Rp);
    (St3,RCond) <- overloadTerm(Cond,Dict,St2,Rp);
    valis (St3,serch(Lc,RPtn,RSrc,RCond))
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
  overloadTerm(letExp(Lc,Lhs,Rhs),Dict,St,Rp) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St,Rp);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1,Rp);
    valis (St2,letExp(Lc,RLhs,RRhs))
  }
  overloadTerm(act(Lc,Act),Dict,St,Rp) => do{
    (St1,RAct) <- overloadAction(Act,Dict,St,Rp);
    valis (St1,act(Lc,RAct))
  }
  overloadTerm(theta(Lc,Nm,Lbled,Gps,Ots,Tp),Dict,St,Rp) => do{
    (RGps,ROts) <- overloadEnvironment(Gps,Ots,Dict,Rp);
    valis (St,theta(Lc,Nm,Lbled,RGps,ROts,Tp))
  }
  overloadTerm(record(Lc,Nm,Lbled,Gps,Ots,Tp),Dict,St,Rp) => do{
    (RGps,ROts) <- overloadEnvironment(Gps,Ots,Dict,Rp);
    valis (St,record(Lc,Nm,Lbled,RGps,ROts,Tp))
  }

  overloadRules([],Els,Dict,St,_) => either((St,Els)).
  overloadRules([eqn(Lc,Ptn,Cond,Exp),..Ts],Els,Dict,St,Rp) => do{
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St,Rp);
    (St2,RCond) <- overloadTerm(Cond,Dict,St1,Rp);
    (St3,RExp) <- overloadTerm(Exp,Dict,St2,Rp);
    overloadRules(Ts,[Els..,eqn(Lc,RPtn,RCond,RExp)],Dict,St3,Rp)
  }

  overloadTerms([],Els,Dict,St,_) => either((St,Els)).
  overloadTerms([T,..Ts],Els,Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    overloadTerms(Ts,[Els..,RT],Dict,St1,Rp)
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
  overloadAction(delayDo(Lc,A1,T1,T2),Dict,St,Rp) => do{
    (St1,RA1) <- overloadAction(A1,Dict,St,Rp);
    valis (St1,delayDo(Lc,RA1,T1,T2))
  }
  overloadAction(assignDo(Lc,Lhs,Rhs,T1,T2),Dict,St,Rp) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St,Rp);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1,Rp);
    valis (St2,assignDo(Lc,RLhs,RRhs,T1,T2))
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
  overloadAction(ifThenDo(Lc,T,L,R,T1,T2),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    (St2,RL) <- overloadAction(L,Dict,St1,Rp);
    (St3,RR) <- overloadAction(R,Dict,St2,Rp);
    valis (St3,ifThenDo(Lc,RT,RL,RR,T1,T2))
  }
  overloadAction(tryCatchDo(Lc,A,C,T1,T2,T3),Dict,St,Rp) => do{
    (St1,RA) <- overloadAction(A,Dict,St,Rp);
    (St2,RC) <- overloadTerm(C,Dict,St1,Rp);
    valis (St2,tryCatchDo(Lc,RA,RC,T1,T2,T3))
  }
  overloadAction(throwDo(Lc,T,T1,T2),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    valis (St1,throwDo(Lc,RT,T1,T2))
  }
  overloadAction(returnDo(Lc,T,T1,T2),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    valis (St1,returnDo(Lc,RT,T1,T2))
  }
  overloadAction(simpleDo(Lc,T,T1,T2),Dict,St,Rp) => do{
    (St1,RT) <- overloadTerm(T,Dict,St,Rp);
    valis (St1,simpleDo(Lc,RT,T1,T2))
  }
    
  resolveContracts:(locn,list[constraint],list[canon],dict,resolveState,reports) =>
    either[reports,(resolveState,list[canon])].
  resolveContracts(_,[],Cx,_,St,Rp) => either((St,Cx)).
  resolveContracts(Lc,[typeConstraint(C),..Cx],Vs,Dict,St,Rp) => do{
    (St0,A) <- resolveContract(Lc,C,Dict,St,Rp);
    resolveContracts(Lc,Cx,[Vs..,A],Dict,St0,Rp)
  }
  
  resolveContract:(locn,tipe,dict,resolveState,reports) => either[reports,(resolveState,canon)].
  resolveContract(Lc,Tp,Dict,St,Rp) => do{
    ImpNm = implementationName(Tp);
    if vrEntry(_,Mk,VTp)^=isVar(ImpNm,Dict) then {
      logMsg("we have implementation $(Mk(Lc,Tp)) for $(Tp)");
      (MTp,VrTp) = freshen(VTp,[],Dict);
      (ITp,Impl) <- manageConstraints(VrTp,[],Lc,Mk(Lc,Tp),Dict,Rp);
      if sameType(ITp,Tp,Dict) then {
	logMsg("we found implementation $(Impl)\:$(ITp)");	
	overloadTerm(Impl,Dict,markResolved(St),Rp)
      } else{
	throw reportError(Rp,"implementation $(ITp) not consistent with $(Tp)",Lc)
      }
    } else{
      throw reportError(Rp,"cannot find an implementation for $(Tp)",Lc)
    }
  }
}
  
