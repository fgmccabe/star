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

  public overloadProgram:(cons[cons[canonDef]],dict,reports) =>
    result[reports,cons[cons[canonDef]]].
  overloadProgram(Gps,Dict,Rp) => 
    overloadGroups(Gps,[],Dict,Rp).

  overloadGroups:(cons[cons[canonDef]],cons[cons[canonDef]],dict,reports) =>
    result[reports,cons[cons[canonDef]]].
  overloadGroups([],Gps,_,_) => do{ valis reverse(Gps)}.
  overloadGroups([Gp,..Gps],RG,Dict,Rp) => do{
    (RGp,NDict) <- overloadGroup(Gp,Dict,Rp);
    overloadGroups(Gps,[RGp,..RG],NDict,Rp)
  }

  overloadGroup:(cons[canonDef],dict,reports)=>result[reports,(cons[canonDef],dict)].
  overloadGroup(Dfs,Dict,Rp) => 
    overloadDefs(Dict,Dfs,[],Rp).

  overloadDefs:(dict,cons[canonDef],cons[canonDef],reports) =>
    result[reports,(cons[canonDef],dict)].
  overloadDefs(Dict,[],Dfx,Rp) => do{ valis (reverse(Dfx),Dict)}.
  overloadDefs(Dict,[D,..Defs],Dfx,Rp) => do{
--    logMsg("overload definition $(D)");
    (DD,DDict) <- overloadDef(Dict,D,Rp);
--    logMsg("overloaded definition $(DD)");
    overloadDefs(DDict,Defs,[DD,..Dfx],Rp)
  }

  overloadDef:(dict,canonDef,reports)=>result[reports,(canonDef,dict)].
  overloadDef(Dict,varDef(Lc,Nm,FullNm,Val,Cx,Tp),Rp) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp).
  overloadDef(Dict,implDef(Lc,Nm,FullNm,Val,Cx,Tp),Rp) =>
    overloadImplDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp).
  overloadDef(Dict,typeDef(Lc,Nm,Tp,TpRl),Rp) => do{ valis (typeDef(Lc,Nm,Tp,TpRl),Dict)}.
  overloadDef(Dict,conDef(Lc,Nm,Tp,TpRl),Rp) => do{ valis (conDef(Lc,Nm,Tp,TpRl),Dict)}.
  overloadDef(Dict,cnsDef(Lc,Nm,FullNm,Tp),Rp) => do{ valis (cnsDef(Lc,Nm,FullNm,Tp),Dict)}.
  
  overloadDef(Dict,Def,Rp) default => do{
    raise reportError(Rp,"cannot overload $(Def)",locOf(Def))
  }
 
  overloadVarDef:(dict,option[locn],string,string,canon,cons[constraint],tipe,reports)=>
    result[reports,(canonDef,dict)].
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,[],Tp,Rp) => do{
    RVal <- overloadTerm(Val,Dict,Rp);
    valis (varDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
  }
  overloadVarDef(Dict,Lc,Nm,FullNm,lambda(Lc,FullNm,Eqns,LTp),Cx,Tp,Rp) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,lambda(Lc,genSym(FullNm),Eqns,LTp),Cx,Tp,Rp).
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp,Rp) => do{
--    logMsg("declare CVars $(Cx)");
    (Cvrs,CDict) .= defineCVars(Lc,Cx,[],Dict);
    RVal <- overloadTerm(Val,CDict,Rp);
    (Qx,Qt) .= deQuant(Tp);
    (_,ITp) .= deConstrain(Qt);
    CTp .= reQuant(Qx,funType(Cx//typeOf,ITp));
    valis (varDef(Lc,Nm,FullNm,lambda(Lc,FullNm,[eqn(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
  }

  overloadImplDef:(dict,option[locn],string,string,canon,cons[constraint],tipe,reports) =>
    result[reports,(canonDef,dict)].
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

    (Cvrs,CDict) .= defineCVars(Lc,Cx,[],Dict);

--    logMsg("cvars = $(Cvrs)");
    RVal <- overloadTerm(Val,CDict,Rp);

    if isEmpty(Cvrs) then {
      CTp .= reQuant(Qx,ITp);
      valis (implDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
    } else {
      CTp .= reQuant(Qx,funType(Cx//genContractType,ITp));
      valis (implDef(Lc,Nm,FullNm,lambda(Lc,FullNm,[eqn(Lc,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
    }
  }

  genContractType(conTract(Nm,Tps,Dps)) => mkConType(Nm,Tps,Dps).

  defineCVars:(option[locn],cons[constraint],cons[canon],dict) => (cons[canon],dict).
  defineCVars(_,[],Vrs,D) => (reverse(Vrs),D).
  defineCVars(Lc,[T,..Tps],Vrs,D) where TpNm .= implementationName(T) && Tp.=typeOf(T) =>
    defineCVars(Lc,Tps,[vr(Lc,TpNm,Tp),..Vrs],
      declareVar(TpNm,Lc,Tp,.none,
	declareImplementation(Lc,TpNm,TpNm,Tp,D))).

  overloadTerm:(canon,dict,reports) => result[reports,canon].
  overloadTerm(C,D,R) => resolveAgain(.inactive,C,resolveTerm(C,D,.inactive),D,R).

  resolveAgain(_,_,(T,.resolved),D,R) =>
    resolveAgain(.inactive,T,resolveTerm(T,D,.inactive),D,R).
  resolveAgain(_,_,(T,.inactive),_,_) => do{ valis T}.
  resolveAgain(active(_,_),_,(_,active(Lc,Msg)),_,R) => do{
    raise reportError(R,Msg,Lc)
  }
  resolveAgain(_,O,(_,active(Lc,Msg)),D,R) =>
    resolveAgain(active(Lc,Msg),O,resolveTerm(O,D,.inactive),D,R).

  resolveTerm:(canon,dict,resolveState) => (canon,resolveState).
  resolveTerm(vr(Lc,Nm,Tp),_,St) => (vr(Lc,Nm,Tp),St).
  resolveTerm(intr(Lc,Ix),_,St) => (intr(Lc,Ix),St).
  resolveTerm(flt(Lc,Dx),_,St) => (flt(Lc,Dx),St).
  resolveTerm(strng(Lc,Sx),_,St) => (strng(Lc,Sx),St).
  resolveTerm(kar(Lc,Cx),_,St) => (kar(Lc,Cx),St).
  resolveTerm(enm(Lc,FullNm,Tp),_,St) => (enm(Lc,FullNm,Tp),St).
  resolveTerm(dot(Lc,Rc,Fld,Tp),Dict,St) => valof{
    (Rc1,St1) .= resolveTerm(Rc,Dict,St);
    valis resolveAccess(Lc,Rc1,Fld,Tp,Dict,St1);
  }
  resolveTerm(update(Lc,R,F,V),Dict,St) => valof{
    (OR,St1) .= resolveTerm(R,Dict,St);
    (OV,St2) .= resolveTerm(V,Dict,St1);
    valis resolveUpdate(Lc,OR,F,OV,Dict,St2);
  }
  resolveTerm(whr(Lc,T,C),Dict,St) => valof{
    (OT,St1) .= resolveTerm(T,Dict,St);
    (OC,St2) .= resolveTerm(C,Dict,St1);
    valis (whr(Lc,OT,OC),St2)
  }
  resolveTerm(mtd(Lc,Nm,Con,Tp),Dict,St) => valof{
--    logMsg("resolve contract $(Con) for $(Nm)");
    (A,St1) .= resolveContract(Lc,Con,Dict,St);
    valis resolveTerm(dot(Lc,A,Nm,Tp),Dict,St1)
  }
  resolveTerm(over(Lc,T,Cx),Dict,St) => valof{
--    logMsg("over $(T)");
    ([A,..Args],St1) .= resolveContracts(Lc,Cx,[],Dict,St);
--    logMsg("contract args $([A,..Args]), St1=$(St1)");
    if mtd(_,Nm,_,MTp) .= T then{
      if _eof(Args) then
	valis resolveTerm(dot(Lc,A,Nm,MTp),Dict,St1)
      else
      valis resolveTerm(apply(Lc,dot(Lc,A,Nm,MTp),tple(Lc,Args),typeOf(T)),Dict,St1)
    }
    else{
      (Tr,St2) .= resolveTerm(T,Dict,St1);
      valis (apply(Lc,Tr,tple(Lc,[A,..Args]),typeOf(T)),St2)
    }
  }
  resolveTerm(apply(lc,Op,Arg,Tp),Dict,St) => valof{
    (ROp,St1) .= resolveTerm(Op,Dict,St);
    (RArgs,St2) .= resolveTerm(Arg,Dict,St1);
    valis (apply(lc,ROp,RArgs,Tp),St2)
  }
  resolveTerm(tple(Lc,Els),Dict,St) => valof{
    (REls,St1) .= resolveTerms(Els,[],Dict,St);
    valis (tple(Lc,REls),St1)
  }

  resolveTerm(match(Lc,Ptn,Src),Dict,St) => valof{
    (RPtn,St1) .= resolveTerm(Ptn,Dict,St);
    (RSrc,St2) .= resolveTerm(Src,Dict,St1);
    valis (match(Lc,RPtn,RSrc),St2)
  }
  resolveTerm(conj(Lc,Lhs,Rhs),Dict,St) => valof{
    (RLhs,St1) .= resolveTerm(Lhs,Dict,St);
    (RRhs,St2) .= resolveTerm(Rhs,Dict,St1);
    valis (conj(Lc,RLhs,RRhs),St2)
  }
  resolveTerm(disj(Lc,Lhs,Rhs),Dict,St) => valof{
    (RLhs,St1) .= resolveTerm(Lhs,Dict,St);
    (RRhs,St2) .= resolveTerm(Rhs,Dict,St1);
    valis (disj(Lc,RLhs,RRhs),St2)
  }
  resolveTerm(implies(Lc,Lhs,Rhs),Dict,St) => valof{
    (RLhs,St1) .= resolveTerm(Lhs,Dict,St);
    (RRhs,St2) .= resolveTerm(Rhs,Dict,St1);
    valis (implies(Lc,RLhs,RRhs),St2)
  }
  resolveTerm(neg(Lc,Rhs),Dict,St) => valof{
    (RRhs,St1) .= resolveTerm(Rhs,Dict,St);
    valis (neg(Lc,RRhs),St1)
  }
  resolveTerm(cond(Lc,Tst,Lhs,Rhs),Dict,St) => valof{
    (RTst,St1) .= resolveTerm(Tst,Dict,St);
    (RLhs,St2) .= resolveTerm(Lhs,Dict,St1);
    (RRhs,St3) .= resolveTerm(Rhs,Dict,St2);
    valis (cond(Lc,RTst,RLhs,RRhs),St3)
  }
  resolveTerm(lambda(Lc,Nm,Rls,Tp),Dict,St) => valof{
    (RRls,St1) .= overloadRules(Rls,[],Dict,St);
--    logMsg("overloaded lambda $(lambda(Lc,Nm,RRls,Tp))");
    valis (lambda(Lc,Nm,RRls,Tp),St1)
  }
  resolveTerm(letExp(Lc,Gp,Decls,Rhs),Dict,St) => valof{
    Rp .= reports([]);
    try{
      (RDfs,RDct) <- overloadGroup(Gp,Dict,Rp);
      (RRhs,St1) .= resolveTerm(Rhs,declareDecls(Decls,Dict),St);
      valis (letExp(Lc,RDfs,Decls,RRhs),St1)
    } catch (E) => do{
      valis (letExp(Lc,Gp,Decls,Rhs),mergeReports(St,E))
    }
  }
  resolveTerm(letRec(Lc,Gp,Decs,Rhs),Dict,St) => valof{
    Rp .= reports([]);
    try{
      (RDfs,RDct) <- overloadGroup(Gp,declareDecls(Decs,Dict),Rp);
      (RRhs,St2) .= resolveTerm(Rhs,RDct,St);
      valis (letRec(Lc,RDfs,Decs,RRhs),St2)
    } catch (E) => do{
      valis (letRec(Lc,Gp,Decs,Rhs),mergeReports(St,E))
    }      
  }
  resolveTerm(csexp(Lc,Gov,Cases,Tp),Dict,St) => valof{
    (RGov,St1) .= resolveTerm(Gov,Dict,St);
    (RCases,St2) .= overloadRules(Cases,[],Dict,St1);
    valis (csexp(Lc,RGov,RCases,Tp),St2)
  }

  overloadRules([],Els,Dict,St) => (reverse(Els),St).
  overloadRules([eqn(Lc,Ptn,.none,Exp),..Ts],Els,Dict,St) => valof{
    (RPtn,St1) .= resolveTerm(Ptn,Dict,St);
    (RExp,St2) .= resolveTerm(Exp,Dict,St1);
    valis overloadRules(Ts,[eqn(Lc,RPtn,.none,RExp),..Els],Dict,St2)
  }
  overloadRules([eqn(Lc,Ptn,some(Wh),Exp),..Ts],Els,Dict,St) => valof{
    (RPtn,St1) .= resolveTerm(Ptn,Dict,St);
    (RExp,St2) .= resolveTerm(Exp,Dict,St1);
    (RWh,St3) .= resolveTerm(Wh,Dict,St2);
    valis overloadRules(Ts,[eqn(Lc,RPtn,some(RWh),RExp),..Els],Dict,St3)
  }
  
  resolveTerms([],Els,Dict,St) => (reverse(Els),St).
  resolveTerms([T,..Ts],Els,Dict,St) => valof{
    (RT,St1) .= resolveTerm(T,Dict,St);
    valis resolveTerms(Ts,[RT,..Els],Dict,St1)
  }
    
  resolveContracts:(option[locn],cons[constraint],cons[canon],dict,resolveState) =>
    (cons[canon],resolveState).
  resolveContracts(_,[],Cx,_,St) => (reverse(Cx),St).
  resolveContracts(Lc,[C,..Cx],Vs,Dict,St) => valof{
    (A,St1) .= resolveContract(Lc,C,Dict,St);
    valis resolveContracts(Lc,Cx,[A,..Vs],Dict,St1)
  }
  
  resolveContract:(option[locn],constraint,dict,resolveState) => (canon,resolveState).
  resolveContract(Lc,Con,Dict,St) => valof{
--    logMsg("resolve contract $(Con)");
    ImpNm .= implementationName(Con);
    Tp .= typeOf(Con);
    if Impl^=findImplementation(Dict,ImpNm) then {
--      logMsg("resolve contract $(Con) using $(Impl)\:$(typeOf(Impl))");
      if sameType(typeOf(Impl),Tp,Dict) then {
--	logMsg("resolving impl var $(Impl)");
	valis resolveTerm(Impl,Dict,markResolved(St))
      } else{
	valis (vd(Lc,Tp),active(Lc,"implementation $(typeOf(Impl)) not consistent with $(Tp)"))
      }
    } else{
      valis (vd(Lc,Tp),active(Lc,"cannot find an implementation for $(Tp)"))
    }
  }

  resolveAccess:(option[locn],canon,string,tipe,dict,resolveState) => (canon,resolveState).
  resolveAccess(Lc,Rc,Fld,Tp,Dict,St) => valof{
--    logMsg("resolve access at $(Lc) of $(Rc).$(Fld), expected type $(Tp)");
    RcTp .= typeOf(Rc);
    if AccFn ^= findAccess(Lc,RcTp,Fld,Dict) then{
--      logMsg("access fun $(AccFn)\:$(typeOf(AccFn))");
      Ft .= newTypeVar("F");
      if sameType(typeOf(AccFn),funType([RcTp],Ft),Dict) then{
	if sameType(Tp,snd(freshen(Ft,Dict)),Dict) then{
	  valis resolveTerm(apply(Lc,AccFn,tple(Lc,[Rc]),Tp),Dict,markResolved(St))
	} else{
	  valis (dot(Lc,Rc,Fld,Tp),
	    active(Lc,"field $(Rc).$(Fld)\:$(Ft) not consistent with required type $(Tp)")).
	}
      } else {
	valis (dot(Lc,Rc,Fld,Tp),
	  active(Lc,"accessor for field $(Rc).$(Fld)\:$(typeOf(AccFn)) for $(RcTp) not consistent with required type $(Tp)"))
      }
    } else{
      valis (dot(Lc,Rc,Fld,Tp),
	active(Lc,"cannot find accessor for field $(Fld) for $(RcTp)"))
    }
  }

  resolveUpdate:(option[locn],canon,string,canon,dict,resolveState) => (canon,resolveState).
  resolveUpdate(Lc,Rc,Fld,Vl,Dict,St) => valof{
--    logMsg("resolve update at $(Lc) of $(Rc).$(Fld)");
    RcTp .= typeOf(Rc);
    if AccFn ^= findUpdate(Lc,RcTp,Fld,Dict) then{
--      logMsg("updater fun $(AccFn)\:$(typeOf(AccFn))");

      Ft .= newTypeVar("F");
      if sameType(typeOf(AccFn),funType([RcTp,Ft],RcTp),Dict) then{
	if sameType(typeOf(Vl),snd(freshen(Ft,Dict)),Dict) then{
--	  logMsg("update $(update(Lc,Rc,Fld,Vl)) resolved to $(apply(Lc,AccFn,tple(Lc,[Rc,Vl]),RcTp))");
	  valis resolveTerm(apply(Lc,AccFn,tple(Lc,[Rc,Vl]),RcTp),Dict,markResolved(St))
	} else{
	  valis (update(Lc,Rc,Fld,Vl),
	    active(Lc,"field $(Fld)\:$(Ft) not consistent with required type $(typeOf(Vl))"))
	}
      }
      else {
	valis (update(Lc,Rc,Fld,Vl),
	  active(Lc,"updater for field $(Fld) for $(RcTp) not consistent with required type $(typeOf(Vl))"))
      }
    } else {
      valis (update(Lc,Rc,Fld,Vl),
	active(Lc,"cannot find updater for field $(Fld) for $(RcTp) not consistent with required type $(typeOf(Vl))"))
    }
  }

  resolveState ::= .inactive |
    .resolved |
    active(option[locn],string).

  markResolved(.inactive) => .resolved.
  markResolved(St) => St.

  mergeReports(_,reports([errorMsg(Lc,Msg),.._])) =>
    active(Lc,Msg).
  mergeReports(St,_) => St.

  implementation display[resolveState] => {
    disp(.inactive) => "inactive".
    disp(.resolved) => "resolved".
    disp(active(Lc,Msg)) => "active $(Lc)\:#(Msg)".
  }
}
  
