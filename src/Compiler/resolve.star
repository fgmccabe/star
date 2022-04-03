star.compiler.resolve{
  import star.

  import star.compiler.canon.
--  import star.compiler.constraints.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.unify.

  public overloadEnvironment:(cons[cons[canonDef]],dict,reports) =>
    either[reports,cons[cons[canonDef]]].
  overloadEnvironment(Gps,Dict,Rp) => 
    overloadGroups(Gps,[],Dict,Rp).

  overloadGroups:(cons[cons[canonDef]],cons[cons[canonDef]],dict,reports) =>
    either[reports,cons[cons[canonDef]]].
  overloadGroups([],Gps,_,_) => either(reverse(Gps)).
  overloadGroups([Gp,..Gps],RG,Dict,Rp) => do{
    (RGp,NDict) <- overloadGroup(Gp,Dict,Rp);
    overloadGroups(Gps,[RGp,..RG],NDict,Rp)
  }

  overloadGroup:(cons[canonDef],dict,reports)=>either[reports,(cons[canonDef],dict)].
  overloadGroup(Dfs,Dict,Rp) => 
    overloadDefs(Dict,Dfs,[],Rp).

  overloadDefs:(dict,cons[canonDef],cons[canonDef],reports) =>
    either[reports,(cons[canonDef],dict)].
  overloadDefs(Dict,[],Dfx,Rp) => either((reverse(Dfx),Dict)).
  overloadDefs(Dict,[D,..Defs],Dfx,Rp) => do{
--    logMsg("overload definition $(D)");
    (DD,DDict) <- overloadDef(Dict,D,Rp);
--    logMsg("overloaded definition $(DD)");
    overloadDefs(DDict,Defs,[DD,..Dfx],Rp)
  }

  overloadDef:(dict,canonDef,reports)=>either[reports,(canonDef,dict)].
  overloadDef(Dict,varDef(Lc,Nm,FullNm,Val,Cx,Tp),Rp) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp).
  overloadDef(Dict,implDef(Lc,Nm,FullNm,Val,Cx,Tp),Rp) =>
    overloadImplDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp).
  overloadDef(Dict,typeDef(Lc,Nm,Tp,TpRl),Rp) => either((typeDef(Lc,Nm,Tp,TpRl),Dict)).
  overloadDef(Dict,conDef(Lc,Nm,Tp,TpRl),Rp) => either((conDef(Lc,Nm,Tp,TpRl),Dict)).
  overloadDef(Dict,cnsDef(Lc,Nm,FullNm,Tp),Rp) => either((cnsDef(Lc,Nm,FullNm,Tp),Dict)).
  
  overloadDef(Dict,Def,Rp) default => do{
    raise reportError(Rp,"cannot overload $(Def)",locOf(Def))
  }
 
  overloadVarDef:(dict,locn,string,string,canon,cons[constraint],tipe,reports)=>
    either[reports,(canonDef,dict)].
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,[],Tp,Rp) => do{
    RVal <- resolveTerm(Val,Dict,Rp);
    valis (varDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
  }
  overloadVarDef(Dict,Lc,Nm,FullNm,lambda(FullNm,Eqns,LTp),Cx,Tp,Rp) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,lambda(genSym(FullNm),Eqns,LTp),Cx,Tp,Rp).
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp) => do{
    (Cvrs,CDict) .= defineCVars(some(Lc),Cx,[],Dict);
    RVal <- resolveTerm(Val,CDict,Rp);
    (Qx,Qt) .= deQuant(Tp);
    (_,ITp) .= deConstrain(Qt);
    CTp .= reQuant(Qx,funType(Cx//typeOf,ITp));
    valis (varDef(Lc,Nm,FullNm,lambda(FullNm,[eqn(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
  }

  overloadImplDef:(dict,locn,string,string,canon,cons[constraint],tipe,reports) =>
    either[reports,(canonDef,dict)].
/*  overloadImplDef(Dict,Lc,Nm,FullNm,Val,[],Tp,Rp) => do{
    logMsg("overload implementation");
    IDict .= undeclareVar(FullNm,Dict);
    RVal <- resolveTerm(Val,IDict,Rp);
    valis (implDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
  }
*/
  overloadImplDef(Dict,Lc,Nm,FullNm,Val,_,Tp,Rp) => do{
--    logMsg("overload implementation $(Nm) = $(Val)\:$(Tp)");
    
    (Qx,Qt) .= deQuant(Tp);
    (Cx,ITp) .= deConstrain(Qt);

--    logMsg("constraints $(Cx)");

    (Cvrs,CDict) .= defineCVars(some(Lc),Cx,[],Dict);

    logMsg("cvars = $(Cvrs)");
    RVal <- resolveTerm(Val,CDict,Rp);

    if isEmpty(Cvrs) then {
      CTp .= reQuant(Qx,ITp);
      valis (implDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
    } else {
      CTp .= reQuant(Qx,funType(Cx//genContractType,ITp));
      valis (implDef(Lc,Nm,FullNm,lambda(FullNm,[eqn(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
    }
  }

  genContractType(conTract(Nm,Tps,Dps)) => mkConType(Nm,Tps,Dps).

  defineCVars:(option[locn],cons[constraint],cons[canon],dict) => (cons[canon],dict).
  defineCVars(_,[],Vrs,D) => (reverse(Vrs),D).
  defineCVars(Lc,[T,..Tps],Vrs,D) where TpNm .= implementationName(T) && Tp.=typeOf(T) =>
    defineCVars(Lc,Tps,[vr(Lc,TpNm,Tp),..Vrs],
      declareVar(TpNm,Lc,Tp,.none,
	declareImplementation(Lc,TpNm,TpNm,Tp,D))).

  resolveTerm:(canon,dict,reports) => either[reports,canon].
  resolveTerm(vr(Lc,Nm,Tp),_,_) => either(vr(Lc,Nm,Tp)).
  resolveTerm(intr(Lc,Ix),_,_) => either(intr(Lc,Ix)).
  resolveTerm(flt(Lc,Dx),_,_) => either(flt(Lc,Dx)).
  resolveTerm(strng(Lc,Sx),_,_) => either(strng(Lc,Sx)).
  resolveTerm(enm(Lc,FullNm,Tp),_,_) => either(enm(Lc,FullNm,Tp)).
  resolveTerm(dot(Lc,Rc,Fld,Tp),Dict,Rp) => do{
    Rc1 <- resolveTerm(Rc,Dict,Rp);
    resolveAccess(Lc,Rc1,Fld,Tp,Dict,Rp);
  }
  resolveTerm(whr(Lc,T,C),Dict,Rp) => do{
    OT <- resolveTerm(T,Dict,Rp);
    OC <- resolveTerm(C,Dict,Rp);
    valis whr(Lc,OT,OC)
  }
  resolveTerm(mtd(Lc,Nm,Con,Tp),Dict,Rp) => do{
    A <- resolveContract(Lc,Con,Dict,Rp);
    valis dot(Lc,A,Nm,Tp)
  }
  resolveTerm(over(Lc,T,Cx),Dict,Rp) => do{
    [A,..Args] <- resolveContracts(Lc,Cx,[],Dict,Rp);
    if mtd(_,Nm,_,MTp) .= T then{
      if _eof(Args) then
	valis dot(Lc,A,Nm,MTp)
      else
      valis apply(Lc,dot(Lc,A,Nm,MTp),tple(Lc,Args),typeOf(T))
    }
    else
    valis apply(Lc,T,tple(Lc,[A,..Args]),typeOf(T))
  }
  resolveTerm(apply(lc,Op,Arg,Tp),Dict,Rp) => do{
    ROp <- resolveTerm(Op,Dict,Rp);
    RArgs <- resolveTerm(Arg,Dict,Rp);
    valis apply(lc,ROp,RArgs,Tp)
  }
  resolveTerm(tple(Lc,Els),Dict,Rp) => do{
    REls <- resolveTerms(Els,[],Dict,Rp);
    valis tple(Lc,REls)
  }
  resolveTerm(match(Lc,Ptn,Src),Dict,Rp) => do{
    RPtn <- resolveTerm(Ptn,Dict,Rp);
    RSrc <- resolveTerm(Src,Dict,Rp);
    valis match(Lc,RPtn,RSrc)
  }
  resolveTerm(conj(Lc,Lhs,Rhs),Dict,Rp) => do{
    RLhs <- resolveTerm(Lhs,Dict,Rp);
    RRhs <- resolveTerm(Rhs,Dict,Rp);
    valis conj(Lc,RLhs,RRhs)
  }
  resolveTerm(disj(Lc,Lhs,Rhs),Dict,Rp) => do{
    RLhs <- resolveTerm(Lhs,Dict,Rp);
    RRhs <- resolveTerm(Rhs,Dict,Rp);
    valis disj(Lc,RLhs,RRhs)
  }
  resolveTerm(implies(Lc,Lhs,Rhs),Dict,Rp) => do{
    RLhs <- resolveTerm(Lhs,Dict,Rp);
    RRhs <- resolveTerm(Rhs,Dict,Rp);
    valis implies(Lc,RLhs,RRhs)
  }
  resolveTerm(neg(Lc,Rhs),Dict,Rp) => do{
    RRhs <- resolveTerm(Rhs,Dict,Rp);
    valis neg(Lc,RRhs)
  }
  resolveTerm(cond(Lc,Tst,Lhs,Rhs),Dict,Rp) => do{
    RTst <- resolveTerm(Tst,Dict,Rp);
    RLhs <- resolveTerm(Lhs,Dict,Rp);
    RRhs <- resolveTerm(Rhs,Dict,Rp);
    valis cond(Lc,RTst,RLhs,RRhs)
  }
  resolveTerm(lambda(Nm,Rls,Tp),Dict,Rp) => do{
    RRls <- overloadRules(Rls,[],Dict,Rp);
    valis lambda(Nm,RRls,Tp)
  }
  resolveTerm(letExp(Lc,Gp,Decls,Rhs),Dict,Rp) => do{
    (RDfs,RDct) <- overloadGroup(Gp,Dict,Rp);
    RRhs <- resolveTerm(Rhs,declareDecls(Decls,Dict),Rp);
    valis letExp(Lc,RDfs,Decls,RRhs)
  }
  resolveTerm(letRec(Lc,Gp,Decs,Rhs),Dict,Rp) => do{
    (RDfs,RDct) <- overloadGroup(Gp,declareDecls(Decs,Dict),Rp);
    RRhs <- resolveTerm(Rhs,RDct,Rp);
    valis letRec(Lc,RDfs,Decs,RRhs)
  }
  resolveTerm(csexp(Lc,Gov,Cases,Tp),Dict,Rp) => do{
    RGov <- resolveTerm(Gov,Dict,Rp);
    RCases <- overloadRules(Cases,[],Dict,Rp);
    valis csexp(Lc,RGov,RCases,Tp)
  }
  resolveTerm(update(Lc,T,C),Dict,Rp) => do{
    OT <- resolveTerm(T,Dict,Rp);
    OC <- resolveTerm(C,Dict,Rp);
    valis update(Lc,OT,OC)
  }

  overloadRules([],Els,Dict,_) => either(reverse(Els)).
  overloadRules([eqn(Lc,Ptn,.none,Exp),..Ts],Els,Dict,Rp) => do{
    RPtn <- resolveTerm(Ptn,Dict,Rp);
    RExp <- resolveTerm(Exp,Dict,Rp);
    overloadRules(Ts,[eqn(Lc,RPtn,.none,RExp),..Els],Dict,Rp)
  }
  overloadRules([eqn(Lc,Ptn,some(Wh),Exp),..Ts],Els,Dict,Rp) => do{
    RPtn <- resolveTerm(Ptn,Dict,Rp);
    RExp <- resolveTerm(Exp,Dict,Rp);
    RWh <- resolveTerm(Wh,Dict,Rp);
    overloadRules(Ts,[eqn(Lc,RPtn,some(RWh),RExp),..Els],Dict,Rp)
  }
  
  resolveTerms([],Els,Dict,_) => either(reverse(Els)).
  resolveTerms([T,..Ts],Els,Dict,Rp) => do{
    RT <- resolveTerm(T,Dict,Rp);
    resolveTerms(Ts,[RT,..Els],Dict,Rp)
  }

  overloadFields:(cons[(string,canon)],cons[(string,canon)],dict,reports) =>
    either[reports,cons[(string,canon)]].
  overloadFields([],Els,Dict,_) => either(reverse(Els)).
  overloadFields([(N,T),..Ts],Els,Dict,Rp) => do{
    RT <- resolveTerm(T,Dict,Rp);
    overloadFields(Ts,[(N,RT),..Els],Dict,Rp)
  }
    
  resolveContracts:(locn,cons[constraint],cons[canon],dict,reports) =>
      either[reports,cons[canon]].
  resolveContracts(_,[],Cx,_,_) => either(reverse(Cx)).
  resolveContracts(Lc,[C,..Cx],Vs,Dict,Rp) => do{
    A <- resolveContract(Lc,C,Dict,Rp);
    resolveContracts(Lc,Cx,[A,..Vs],Dict,Rp)
  }
  
  resolveContract:(locn,constraint,dict,reports) => either[reports,canon].
  resolveContract(Lc,Con,Dict,Rp) => do{
    ImpNm .= implementationName(Con);
    Tp .= typeOf(Con);
    if Impl^=findImplementation(Dict,ImpNm) then {
      logMsg("resolve contract $(Con) using $(Impl)");
      if sameType(typeOf(Impl),Tp,Dict) then {
	logMsg("resolving impl var $(Impl)");
	resolveTerm(Impl,Dict,Rp)
      } else{
	raise reportError(Rp,"implementation $(typeOf(Impl)) not consistent with $(Tp)",Lc)
      }
    } else{
      raise reportError(Rp,"cannot find an implementation for $(Tp)",Lc)
    }
  }

  resolveAccess:(locn,canon,string,tipe,dict,reports) => either[reports,canon].
  resolveAccess(Lc,Rc,Fld,Tp,Dict,Rp) => do{
    logMsg("resolve access at $(Lc) of $(Fld) in $(Rc)\:$(typeOf(Rc)), expected type $(Tp)");
    RcTp .= typeOf(Rc);
    if AccFn ^= findAccess(Lc,RcTp,Fld,Dict) then{
      logMsg("access fun $(AccFn)\:$(typeOf(AccFn))");
      if sameType(typeOf(AccFn),funType([RcTp],Tp),Dict) then{
	resolveTerm(apply(Lc,AccFn,tple(Lc,[Rc]),Tp),Dict,Rp)
      } else {
	raise reportError(Rp,"cannot find accessor for field $(Fld) for $(RcTp) not consistent with required type $(Tp)",Lc)
      }
    } else{
      raise reportError(Rp,"cannot find accessor for field $(Fld) for $(RcTp)",Lc)
    }
  }
}
  
