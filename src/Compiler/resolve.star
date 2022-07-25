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
  overloadDef(varDef(Lc,Nm,FullNm,Val,Cx,Tp),Dict) =>
    overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp).
  overloadDef(implDef(Lc,Nm,FullNm,Val,Cx,Tp),Dict) =>
    overloadImplDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp).
  overloadDef(typeDef(Lc,Nm,Tp,TpRl),Dict) => (typeDef(Lc,Nm,Tp,TpRl),Dict).
  overloadDef(conDef(Lc,Nm,Tp,TpRl),Dict) => (conDef(Lc,Nm,Tp,TpRl),Dict).
  overloadDef(cnsDef(Lc,Nm,FullNm,Tp),Dict) => (cnsDef(Lc,Nm,FullNm,Tp),Dict).
  overloadDef(Def,Dict) default => valof{
    reportError("cannot overload $(Def)",locOf(Def));
    valis (Def,Dict)
  }
 
  overloadVarDef:(dict,option[locn],string,string,canon,cons[constraint],tipe)=>
    (canonDef,dict).
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,[],Tp) => 
    (varDef(Lc,Nm,FullNm,overloadTerm(Val,Dict),[],Tp),Dict).
  overloadVarDef(Dict,Lc,Nm,FullNm,Val,Cx,Tp) => valof{
--    logMsg("declare CVars $(Cx)");
    (Cvrs,CDict) = defineCVars(Lc,Cx,[],Dict);
    RVal = overloadTerm(Val,CDict);
    (Qx,Qt) = deQuant(Tp);
    (_,ITp) = deConstrain(Qt);
    CTp = reQuant(Qx,funType(Cx//typeOf,ITp));
    valis (varDef(Lc,Nm,FullNm,lambda(Lc,FullNm,[rule(Lc,.false,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
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
    RVal = overloadTerm(Val,CDict);

    if isEmpty(Cvrs) then {
      CTp = reQuant(Qx,ITp);
      valis (implDef(Lc,Nm,FullNm,RVal,[],Tp),Dict)
    } else {
      CTp = reQuant(Qx,funType(Cx//genContractType,ITp));
      valis (implDef(Lc,Nm,FullNm,lambda(Lc,FullNm,[rule(Lc,.false,tple(Lc,Cvrs),.none,RVal)],CTp),[],Tp),Dict)
    }
  }

  genContractType(conTract(Nm,Tps,Dps)) => mkConType(Nm,Tps,Dps).

  defineCVars:(option[locn],cons[constraint],cons[canon],dict) => (cons[canon],dict).
  defineCVars(_,[],Vrs,D) => (reverse(Vrs),D).
  defineCVars(Lc,[T,..Tps],Vrs,D) where TpNm .= implementationName(T) && Tp.=typeOf(T) =>
    defineCVars(Lc,Tps,[vr(Lc,TpNm,Tp),..Vrs],
      declareVar(TpNm,Lc,Tp,.none,
	declareImplementation(Lc,TpNm,TpNm,Tp,D))).

  overloadTerm:(canon,dict) => canon.
  overloadTerm(C,D) => resolveAgain(.inactive,C,resolveTerm(C,D,.inactive),D).

  resolveAgain(_,_,(T,.resolved),D) =>
    resolveAgain(.inactive,T,resolveTerm(T,D,.inactive),D).
  resolveAgain(_,_,(T,.inactive),_) => T.
  resolveAgain(active(_,_),_,(T,active(Lc,Msg)),_) => valof{
    reportError(Msg,Lc);
    valis T
  }
  resolveAgain(_,O,(_,active(Lc,Msg)),D) =>
    resolveAgain(active(Lc,Msg),O,resolveTerm(O,D,.inactive),D).

  resolveTerm:(canon,dict,resolveState) => (canon,resolveState).
  resolveTerm(vr(Lc,Nm,Tp),_,St) => (vr(Lc,Nm,Tp),St).
  resolveTerm(intr(Lc,Ix),_,St) => (intr(Lc,Ix),St).
  resolveTerm(bintr(Lc,Ix),_,St) => (bintr(Lc,Ix),St).
  resolveTerm(flt(Lc,Dx),_,St) => (flt(Lc,Dx),St).
  resolveTerm(strng(Lc,Sx),_,St) => (strng(Lc,Sx),St).
  resolveTerm(kar(Lc,Cx),_,St) => (kar(Lc,Cx),St).
  resolveTerm(enm(Lc,FullNm,Tp),_,St) => (enm(Lc,FullNm,Tp),St).
  resolveTerm(dot(Lc,Rc,Fld,Tp),Dict,St) => valof{
    (Rc1,St1) = resolveTerm(Rc,Dict,St);
    valis resolveDot(Lc,Rc1,Fld,Tp,Dict,St1);
  }
  resolveTerm(update(Lc,R,F,V),Dict,St) => valof{
    (OR,St1) = resolveTerm(R,Dict,St);
    (OV,St2) = resolveTerm(V,Dict,St1);
    valis resolveUpdate(Lc,OR,F,OV,Dict,St2);
  }
  resolveTerm(owpen(Lc,Rc),Dict,St) => valof{
    (Rc1,St1) = resolveTerm(Rc,Dict,St);
    valis (owpen(Lc,Rc1),St1);
  }
  resolveTerm(whr(Lc,T,C),Dict,St) => valof{
    (OT,St1) = resolveTerm(T,Dict,St);
    (OC,St2) = resolveTerm(C,Dict,St1);
    valis (whr(Lc,OT,OC),St2)
  }
  resolveTerm(mtd(Lc,Nm,Con,Tp),Dict,St) => valof{
    logMsg("resolve contract $(Con) for $(Nm)");
    (A,St1) = resolveContract(Lc,Con,Dict,St);
    valis resolveTerm(dot(Lc,A,Nm,Tp),Dict,St1)
  }
  resolveTerm(over(Lc,Tp,Cx),Dict,St) => valof{
--    logMsg("over $(T)");
    (DArgs,St1) = resolveContracts(Lc,Cx,[],Dict,St);
    (OverOp,NArgs,St2) = resolveRef(T,DArgs,[],Dict,St1);
    valis (overApply(Lc,OverOp,NArgs,Tp),markResolved(St2))
  }
  resolveTerm(overaccess(Lc,T,RcTp,Fld,FldTp),Dict,St) => valof{
    logMsg("over access $(T)");
    AccessOp = resolveAccess(Lc,RcTp,Fld,FldTp,Dict,St,St1);
    valis (T,St1)
  }
  resolveTerm(apply(lc,over(OLc,T,Cx),Args,Tp),Dict,St) => valof{
    (DArgs,St1) = resolveContracts(Lc,Cx,[],Dict,St);
    (RArgs,St2) = resolveTerm(Args,Dict,St1);
    (OverOp,NArgs,St3) =overloadRef(T,DArgs,RArgs,Dict,St2);
    valis (apply(lc,OverOp,NArgs,Tp),markResolved(St3))
  }
  resolveTerm(apply(lc,Op,Arg,Tp),Dict,St) => valof{
    (ROp,St1) = resolveTerm(Op,Dict,St);
    (RArgs,St2) = resolveTerm(Arg,Dict,St1);
    valis (apply(lc,ROp,RArgs,Tp),St2)
  }
  resolveTerm(tple(Lc,Els),Dict,St) => valof{
    (REls,St1) = resolveTerms(Els,[],Dict,St);
    valis (tple(Lc,REls),St1)
  }
  resolveTerm(match(Lc,Ptn,Src),Dict,St) => valof{
    (RPtn,St1) = resolveTerm(Ptn,Dict,St);
    (RSrc,St2) = resolveTerm(Src,Dict,St1);
    valis (match(Lc,RPtn,RSrc),St2)
  }
  resolveTerm(conj(Lc,Lhs,Rhs),Dict,St) => valof{
    (RLhs,St1) = resolveTerm(Lhs,Dict,St);
    (RRhs,St2) = resolveTerm(Rhs,Dict,St1);
    valis (conj(Lc,RLhs,RRhs),St2)
  }
  resolveTerm(disj(Lc,Lhs,Rhs),Dict,St) => valof{
    (RLhs,St1) = resolveTerm(Lhs,Dict,St);
    (RRhs,St2) = resolveTerm(Rhs,Dict,St1);
    valis (disj(Lc,RLhs,RRhs),St2)
  }
  resolveTerm(implies(Lc,Lhs,Rhs),Dict,St) => valof{
    (RLhs,St1) = resolveTerm(Lhs,Dict,St);
    (RRhs,St2) = resolveTerm(Rhs,Dict,St1);
    valis (implies(Lc,RLhs,RRhs),St2)
  }
  resolveTerm(neg(Lc,Rhs),Dict,St) => valof{
    (RRhs,St1) = resolveTerm(Rhs,Dict,St);
    valis (neg(Lc,RRhs),St1)
  }
  resolveTerm(cond(Lc,Tst,Lhs,Rhs),Dict,St) => valof{
    (RTst,St1) = resolveTerm(Tst,Dict,St);
    (RLhs,St2) = resolveTerm(Lhs,Dict,St1);
    (RRhs,St3) = resolveTerm(Rhs,Dict,St2);
    valis (cond(Lc,RTst,RLhs,RRhs),St3)
  }
  resolveTerm(lambda(Lc,Nm,Rls,Tp),Dict,St) => valof{
    (RRls,St1) = overloadRules(Rls,[],Dict,St);
--    logMsg("overloaded lambda $(lambda(Lc,Nm,RRls,Tp))");
    valis (lambda(Lc,Nm,RRls,Tp),St1)
  }
  resolveTerm(letExp(Lc,Gp,Decls,Rhs),Dict,St) => valof{
    (RDfs,RDct) = overloadGroup(Gp,Dict);
    (RRhs,St1) = resolveTerm(Rhs,declareDecls(Decls,Dict),St);
    valis (letExp(Lc,RDfs,Decls,RRhs),St1)
  }
  resolveTerm(letRec(Lc,Gp,Decs,Rhs),Dict,St) => valof{
    (RDfs,RDct) = overloadGroup(Gp,declareDecls(Decs,Dict));
    (RRhs,St2) = resolveTerm(Rhs,RDct,St);
    valis (letRec(Lc,RDfs,Decs,RRhs),St2)
  }
  resolveTerm(csexp(Lc,Gov,Cases,Tp),Dict,St) => valof{
    (RGov,St1) = resolveTerm(Gov,Dict,St);
    (RCases,St2) = overloadRules(Cases,[],Dict,St1);
    valis (csexp(Lc,RGov,RCases,Tp),St2)
  }

  resolveRef(mtd(Lc,Nm,Tp),[DT,..Ds],Args,Dict,St) => valof{
    (OverOp,St1) = resolveDot(Lc,DT,Nm,Tp,Dict,St);
    valis (OverOp,Ds++Args,St1)
  }
  resolveRef(C,DArgs,Args,_,St) default =>
    (C,DArgs++Args,St).

  /*
  
    ([A,..Args],St1) .= resolveContracts(Lc,Cx,[],Dict,St);
--    logMsg("contract args $([A,..Args]), St1=$(St1)");
    if mtd(_,Nm,_,MTp) .= T then{
      if _eof(Args) then
	valis resolveTerm(dot(Lc,A,Nm,MTp),Dict,St1)
      else
      valis resolveTerm(apply(Lc,dot(Lc,A,Nm,MTp),tple(Lc,Args),typeOf(T)),Dict,St1)
    }
    else{
      (Tr,St2) = resolveTerm(T,Dict,St1);
      valis (apply(Lc,Tr,tple(Lc,[A,..Args]),typeOf(T)),St2)
    }
  }
*/
    
  overloadRules([],Els,Dict,St) => (reverse(Els),St).
  overloadRules([rule(Lc,Deflt,Ptn,.none,Exp),..Ts],Els,Dict,St) => valof{
    (RPtn,St1) = resolveTerm(Ptn,Dict,St);
    (RExp,St2) = resolveTerm(Exp,Dict,St1);
    valis overloadRules(Ts,[rule(Lc,Deflt,RPtn,.none,RExp),..Els],Dict,St2)
  }
  overloadRules([rule(Lc,Deflt,Ptn,some(Wh),Exp),..Ts],Els,Dict,St) => valof{
    (RPtn,St1) = resolveTerm(Ptn,Dict,St);
    (RExp,St2) = resolveTerm(Exp,Dict,St1);
    (RWh,St3) = resolveTerm(Wh,Dict,St2);
    valis overloadRules(Ts,[rule(Lc,Deflt,RPtn,some(RWh),RExp),..Els],Dict,St3)
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
    (A,St1) = resolveContract(Lc,C,Dict,St);
    valis resolveContracts(Lc,Cx,[A,..Vs],Dict,St1)
  }
  
  resolveContract:(option[locn],constraint,dict,resolveState) => (canon,resolveState).
  resolveContract(Lc,Con,Dict,St) => valof{
--    logMsg("resolve contract $(Con)");
    ImpNm = implementationName(Con);
    Tp = typeOf(Con);
    if Impl^=findImplementation(Dict,ImpNm) then {
--      logMsg("resolve contract $(Con) using $(Impl)\:$(typeOf(Impl))");
      if sameType(typeOf(Impl),Tp,Dict) then {
--	logMsg("resolving impl var $(Impl)");
	valis resolveTerm(Impl,Dict,markResolved(St))
      } else{
	valis (vd(Lc),active(Lc,"implementation $(typeOf(Impl)) not consistent with $(Tp)"))
      }
    } else{
      valis (vd(Lc),active(Lc,"cannot find an implementation for $(Tp)"))
    }
  }

  resolveDot:(option[locn],canon,string,tipe,dict,resolveState) => (canon,resolveState).
  resolveDot(Lc,Rc,Fld,Tp,Dict,St) => valof{
--    logMsg("resolve access at $(Lc) of $(Rc).$(Fld), expected type $(Tp)");
    RcTp = typeOf(Rc);
    if AccFn ^= findAccess(Lc,RcTp,Fld,Dict) then{
--      logMsg("access fun $(AccFn)\:$(typeOf(AccFn))");
      Ft = newTypeVar("F");
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
    RcTp = typeOf(Rc);
    if AccFn ^= findUpdate(Lc,RcTp,Fld,Dict) then{
--      logMsg("updater fun $(AccFn)\:$(typeOf(AccFn))");

      Ft = newTypeVar("F");
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

  implementation display[resolveState] => {
    disp(.inactive) => "inactive".
    disp(.resolved) => "resolved".
    disp(active(Lc,Msg)) => "active $(Lc)\:#(Msg)".
  }
}
  
