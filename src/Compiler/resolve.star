star.compiler.resolve{
  import star.

  import star.compiler.canon.
  import star.compiler.constraints.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
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
    TDict .= declareImplementations(Gps,Dict);
    _ <- resolveConstraints(Gps,TDict,Rp);
    overloadGroups(Gps,[],TDict,Rp)
  }

  declareImplementations([],Dict) => Dict.
  declareImplementations([Gp,..Gps],Dict) =>
    declareImplementations(Gps,declareImplementationsInGroup(Gp,Dict)).

  declareImplementationsInGroup:(cons[canonDef],dict) => dict.
  declareImplementationsInGroup([],Dict) => Dict.
  declareImplementationsInGroup([implDef(Lc,_,FullNm,_,_,Tp),..Gp],Dict) =>
    declareImplementationsInGroup(Gp,
      declareVar(FullNm,some(Lc),Tp,.none,
	declareImplementation(FullNm,Tp,Dict))).
  declareImplementationsInGroup([_,..Gp],Dict) => declareImplementationsInGroup(Gp,Dict).

  overloadGroups:(cons[cons[canonDef]],cons[cons[canonDef]],dict,reports) =>
    either[reports,cons[cons[canonDef]]].
  overloadGroups([],Gps,_,_) => either(reverse(Gps)).
  overloadGroups([Gp,..Gps],RG,Dict,Rp) => do{
    (RGp,GDict) <- overloadGroup(Gp,Dict,Rp);
    overloadGroups(Gps,[RGp,..RG],GDict,Rp)
  }

  public overloadGroup:(cons[canonDef],dict,reports)=>either[reports,(cons[canonDef],dict)].
  overloadGroup(Dfs,Dict,Rp) => do{
    TDict .= declareImplementationsInGroup(Dfs,Dict);
    overloadDefs(TDict,Dfs,[],Rp)
  }

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
    overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx//(contractConstraint(CTp))=>CTp,Tp,Rp).
  overloadDef(Dict,implDef(Lc,Nm,FullNm,Val,Cx,Tp),Rp) =>
    overloadImplDef(Dict,Lc,Nm,FullNm,Val,Cx//(contractConstraint(CTp))=>CTp,Tp,Rp).
  overloadDef(Dict,typeDef(Lc,Nm,Tp,TpRl),Rp) => do{
    valis (typeDef(Lc,Nm,Tp,TpRl),declareType(Nm,some(Lc),Tp,TpRl,Dict))
  }
  overloadDef(Dict,Def,Rp) default => either((Def,Dict)).
 
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,[],Tp,Rp) => do{
    RVal <- resolveTerm(Val,Dict,Rp);
    valis (varDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
  }
  overloadVarDef(Dict,Lc,Nm,FullNm,lambda(FullNm,Eqns,LTp),Cx,Tp,Rp) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,lambda(genSym(FullNm),Eqns,LTp),Cx,Tp,Rp).
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp) => do{
    (Cvrs,CDict) .= defineCVars(Lc,Cx,[],Dict);
    RVal <- resolveTerm(Val,CDict,Rp);
    (Qx,Qt) .= deQuant(Tp);
    (_,ITp) .= deConstrain(Qt);
    CTp .= reQuant(Qx,funType(Cx,ITp));
    valis (varDef(Lc,Nm,FullNm,lambda(FullNm,[eqn(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
  }

  overloadImplDef(Dict,Lc,Nm,FullNm,Val,[],Tp,Rp) => do{
    IDict .= undeclareVar(FullNm,Dict);
    RVal <- resolveTerm(Val,IDict,Rp);
    valis (implDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
  }
  overloadImplDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp) => do{
--    logMsg("overloading implementation $(Nm)");
--    logMsg("Cx= $(Cx)");
    (Cvrs,CDict) .= defineCVars(Lc,Cx,[],Dict);
--    logMsg("Cvars=$(Cvrs)");
    RVal <- resolveTerm(Val,CDict,Rp);
    (Qx,Qt) .= deQuant(Tp);
    (_,ITp) .= deConstrain(Qt);
    CTp .= reQuant(Qx,funType(Cx,ITp));
    valis (implDef(Lc,Nm,FullNm,lambda(FullNm,[eqn(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
  }

  defineCVars:(locn,cons[tipe],cons[canon],dict) => (cons[canon],dict).
  defineCVars(_,[],Vrs,D) => (reverse(Vrs),D).
  defineCVars(Lc,[T,..Tps],Vrs,D) where TpNm .= implementationName(T) =>
    defineCVars(Lc,Tps,[vr(Lc,TpNm,T),..Vrs],declareVar(TpNm,some(Lc),T,.none,D)).

  resolveState ::= .inactive | .resolved | active(locn,string).

  implementation display[resolveState] => {.
    disp(.inactive) => ss("inactive").
    disp(.resolved) => ss("resolved").
    disp(active(Lc,Msg)) => ssSeq([ss("active "),disp(Lc)])
  .}

  markResolved(.inactive) => .resolved.
  markResolved(St) default => St.

  public resolveTerm(Term,Dict,Rp) => do{
    try{
      (St,RTerm) <- overloadTerm(Term,Dict,.inactive);
      valis RTerm
--      logMsg("result of resolving $(Term) is $(RTerm)\:$(St)");
--      resolveAgain(.inactive,St,RTerm,Dict)
    } catch ((Lc,Ms)) => do{
      throw reportError(Rp,Ms,Lc)
    }
  }

  resolveAgain(_,.resolved,Term,Dict) => do{
    (St,RTerm) <- overloadTerm(Term,Dict,.inactive);
    resolveAgain(.inactive,St,RTerm,Dict)
  }
  resolveAgain(_,.inactive,Term,_) => either(Term).
  resolveAgain(active(_,_),active(Lc,Msg),_,_) => other((Lc,Msg)).
  resolveAgain(_,active(Lc,Msg),Term,Dict) => do{
    (St,RTerm) <- overloadTerm(Term,Dict,.inactive);
--    logMsg("try again to resolve $(Term)");
    resolveAgain(active(Lc,Msg),St,RTerm,Dict)
  }

  overloadTerm:(canon,dict,resolveState) => either[(locn,string),(resolveState,canon)].
  overloadTerm(vr(Lc,Nm,Tp),_,St) => either((St,vr(Lc,Nm,Tp))).
  overloadTerm(intr(Lc,Ix),_,St) => either((St,intr(Lc,Ix))).
  overloadTerm(flt(Lc,Dx),_,St) => either((St,flt(Lc,Dx))).
  overloadTerm(strng(Lc,Sx),_,St) => either((St,strng(Lc,Sx))).
  overloadTerm(enm(Lc,FullNm,Tp),_,St) => either((St,enm(Lc,FullNm,Tp))).
  overloadTerm(dot(Lc,Rc,Fld,Tp),Dict,St) => do{
    (Stx,Rc1) <- overloadTerm(Rc,Dict,St);
    valis (Stx,dot(Lc,Rc1,Fld,Tp))
  }
  overloadTerm(whr(Lc,T,C),Dict,St) => do{
--    logMsg("normalize where term $(whr(Lc,T,C)), $(St)");
    (St1,OT) <- overloadTerm(T,Dict,St);
--    logMsg("normalize condition $(C), $(St1)");
    (Stx,OC) <- overloadTerm(C,Dict,St1);
--    logMsg("normalized where $(OC) : $(Stx)");
    valis (Stx,whr(Lc,OT,OC))
  }
  overloadTerm(mtd(Lc,Nm,Con,Tp),Dict,St) => do{
--    logMsg("overloading $(mtd(Lc,Nm,Con,Tp))");
    (St1,A) <- resolveContract(Lc,Con,Dict,St);
    valis (St1,dot(Lc,A,Nm,Tp))
  }
  overloadTerm(over(Lc,T,Tp,Cx),Dict,St) => do{
--    logMsg("overloading $(over(Lc,T,Tp,Cx)) |: $(T)\:$(Tp)");
    try{
      (St1,[A,..Args]) <- resolveContracts(Lc,Cx,[],Dict,St);
--      logMsg("after contracts resolved $(St1)");
      if mtd(_,Nm,_,MTp) .= T then{
	if _eof(Args) then
	  valis (St1,dot(Lc,A,Nm,Tp))
	else
	valis (St1,apply(Lc,dot(Lc,A,Nm,MTp),tple(Lc,Args),Tp))
      }
      else
      valis (St1,apply(Lc,T,tple(Lc,[A,..Args]),Tp))
    } catch ((ELc,E)) => do{
      valis (active(ELc,E),over(Lc,T,Tp,Cx))
    }
  }
  overloadTerm(apply(lc,Op,Arg,Tp),Dict,St) => do{
    (St1,ROp) <- overloadTerm(Op,Dict,St);
    (St2,RArgs) <- overloadTerm(Arg,Dict,St1);
    valis (St2,apply(lc,ROp,RArgs,Tp))
  }
  overloadTerm(tple(Lc,Els),Dict,St) => do{
    (Stx,REls) <- overloadTerms(Els,[],Dict,St);
    valis (Stx,tple(Lc,REls))
  }
  overloadTerm(match(Lc,Ptn,Src),Dict,St) => do{
--    logMsg("normalize match $(match(Lc,Ptn,Src)), St=$(St)");
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St);
--    logMsg("match pattern $(RPtn)\:$(St1)");
    (St2,RSrc) <- overloadTerm(Src,Dict,St1);
--    logMsg("match exp $(RSrc)\:$(St2)");
    valis (St2,match(Lc,RPtn,RSrc))
  }
  overloadTerm(conj(Lc,Lhs,Rhs),Dict,St) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1);
    valis (St2,conj(Lc,RLhs,RRhs))
  }
  overloadTerm(disj(Lc,Lhs,Rhs),Dict,St) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1);
    valis (St2,disj(Lc,RLhs,RRhs))
  }
  overloadTerm(implies(Lc,Lhs,Rhs),Dict,St) => do{
    (St1,RLhs) <- overloadTerm(Lhs,Dict,St);
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St1);
    valis (St2,implies(Lc,RLhs,RRhs))
  }
  overloadTerm(neg(Lc,Rhs),Dict,St) => do{
    (St2,RRhs) <- overloadTerm(Rhs,Dict,St);
    valis (St2,neg(Lc,RRhs))
  }
  overloadTerm(cond(Lc,Tst,Lhs,Rhs),Dict,St) => do{
    (St1,RTst) <- overloadTerm(Tst,Dict,St);
    (St2,RLhs) <- overloadTerm(Lhs,Dict,St1);
    (St3,RRhs) <- overloadTerm(Rhs,Dict,St2);
    valis (St3,cond(Lc,RTst,RLhs,RRhs))
  }
  overloadTerm(lambda(Nm,Rls,Tp),Dict,St) => do{
    (Stx,RRls) <- overloadRules(Rls,[],Dict,St);
    valis (Stx,lambda(Nm,RRls,Tp))
  }
  overloadTerm(letExp(Lc,Gp,Rhs),Dict,St) => do{
    try{
--    logMsg("overload let $(letRec(Lc,Gp,Rhs))");
      Rp .= reports([]);
      (RDfs,RDct) <- overloadGroup(Gp,Dict,Rp);
--    logMsg("new dict $(RDct)");
      (St2,RRhs) <- overTerm(Rhs,RDct,St,Rp);
      valis (St2,letExp(Lc,RDfs,RRhs))
    } catch(reports([errorMsg(ELc,Msg),.._])) => do{
      valis (active(ELc,Msg),letExp(Lc,Gp,Rhs))
    }
  }
  overloadTerm(letRec(Lc,Gp,Rhs),Dict,St) => do{
    try{
--    logMsg("overload letrec $(letRec(Lc,Gp,Rhs))");
      Rp .= reports([]);
      (RDfs,RDct) <- overloadGroup(Gp,Dict,Rp);
--    logMsg("new dict $(RDct)");
      (St2,RRhs) <- overTerm(Rhs,RDct,St,Rp);
      valis (St2,letRec(Lc,RDfs,RRhs))
    } catch (reports([errorMsg(Lc,Msg),.._])) => do{
      valis (active(Lc,Msg),letRec(Lc,Gp,Rhs))
    }
  }
  overloadTerm(csexp(Lc,Gov,Cases,Tp),Dict,St) => do{
    (St1,RGov) <- overloadTerm(Gov,Dict,St);
    (St2,RCases) <- overloadRules(Cases,[],Dict,St1);
    valis (St2,csexp(Lc,RGov,RCases,Tp))
  }
  overloadTerm(record(Lc,Nm,Fields,Tp),Dict,St) => do{
    (Stx,RFields) <- overloadFields(Fields,[],Dict,St);
    valis (St,record(Lc,Nm,RFields,Tp))
  }
  overloadTerm(update(Lc,T,C),Dict,St) => do{
    (St1,OT) <- overloadTerm(T,Dict,St);
    (Stx,OC) <- overloadTerm(C,Dict,St1);
    valis (Stx,update(Lc,OT,OC))
  }

  overTerm:(canon,dict,resolveState,reports) => either[reports,(resolveState,canon)].
  overTerm(T,Dict,St,Rp) => do{
    try{
      overloadTerm(T,Dict,St)
    } catch((Lc,Msg))=>do{
      throw reportError(Rp,Msg,Lc)
    }
  }

  overloadRules([],Els,Dict,St) => either((St,reverse(Els))).
  overloadRules([eqn(Lc,Ptn,.none,Exp),..Ts],Els,Dict,St) => do{
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St);
    (St2,RExp) <- overloadTerm(Exp,Dict,St1);
    overloadRules(Ts,[eqn(Lc,RPtn,.none,RExp),..Els],Dict,St2)
  }
  overloadRules([eqn(Lc,Ptn,some(Wh),Exp),..Ts],Els,Dict,St) => do{
    (St1,RPtn) <- overloadTerm(Ptn,Dict,St);
    (St2,RExp) <- overloadTerm(Exp,Dict,St1);
    (Stx,RWh) <- overloadTerm(Wh,Dict,St2);
    overloadRules(Ts,[eqn(Lc,RPtn,some(RWh),RExp),..Els],Dict,Stx)
  }
  
  overloadTerms([],Els,Dict,St) => either((St,reverse(Els))).
  overloadTerms([T,..Ts],Els,Dict,St) => do{
    (St1,RT) <- overloadTerm(T,Dict,St);
    overloadTerms(Ts,[RT,..Els],Dict,St1)
  }

  overloadFields([],Els,Dict,St) => either((St,reverse(Els))).
  overloadFields([(N,T),..Ts],Els,Dict,St) => do{
    (St1,RT) <- overloadTerm(T,Dict,St);
    overloadFields(Ts,[(N,RT),..Els],Dict,St1)
  }
    
  resolveContracts:(locn,cons[constraint],cons[canon],dict,resolveState) =>
      either[(locn,string),(resolveState,cons[canon])].
  resolveContracts(_,[],Cx,_,St) => either((St,reverse(Cx))).
  resolveContracts(Lc,[contractConstraint(C),..Cx],Vs,Dict,St) => do{
    (St0,A) <- resolveContract(Lc,C,Dict,St);
    resolveContracts(Lc,Cx,[A,..Vs],Dict,St0)
  }
  
  resolveContract:(locn,tipe,dict,resolveState) => either[(locn,string),(resolveState,canon)].
  resolveContract(Lc,Tp,Dict,St) => do{
    ImpNm .= implementationName(Tp);
--    logMsg("looking for implementation $(Tp) - $(ImpNm)");
    if Impl^=findVar(Lc,ImpNm,Dict) then {
--      logMsg("we have implementation $(Impl)\:$(typeOf(Impl))");
      if sameType(typeOf(Impl),Tp,Dict) then {
	overloadTerm(Impl,Dict,markResolved(St))
      } else{
	  throw (Lc,"implementation $(typeOf(Impl)) not consistent with $(Tp)")
      }
    } else{
	throw (Lc,"cannot find an implementation for $(Tp)")
    }
  }
}
  
