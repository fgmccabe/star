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

  public overload:(dict,list[canonDef],reports) =>
    either[reports,list[canonDef]].
  overload(Specs,Defs,Rp) => overloadDefs(Specs,Defs,[],Rp).

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

  resolveTerm(Term,Dict,Rp) => do{
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
    OverOp = (mtd(_,Nm,Tp) .= T ? dot(Lc,A,Nm,Tp) || T);
    valis (St1,apply(Lc,OverOp,tple(Lc,[A,..Args]),Tp))
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
    logMsg("we have implementation name $(ImpNm) for $(Tp)");
    throw reportError(Rp,"cannot resolve $(Tp)",Lc)
  }
}
  
