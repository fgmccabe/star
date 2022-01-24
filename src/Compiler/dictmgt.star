star.compiler.dict.mgt{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.unify.

  isVar:(string,dict) => option[vrEntry].
  isVar(Nm,_) where (Tp,_) ^= intrinsic(Nm) =>
    some(vrEntry(.none,(L,E)=>refreshVar(L,Nm,Tp,E),Tp,.none)).
  isVar(Nm,_) where Tp ^= escapeType(Nm) => some(vrEntry(.none,(L,E)=>refreshVar(L,Nm,Tp,E),Tp,.none)).
  isVar(Nm,[]) => .none.
  isVar(Nm,[scope(_,Vrs,_,_),.._]) where Entry^=Vrs[Nm] => some(Entry).
  isVar(Nm,[_,..D]) => isVar(Nm,D).

  public findVar:(locn,string,dict) => option[canon].
  findVar(Lc,Nm,Dict) where vrEntry(_,Mk,Tp,_) ^= isVar(Nm,Dict) => some(Mk(Lc,Dict)).
  findVar(_,_,_) default => .none.

  public findVarFace:(locn,string,dict) => option[(canon,tipe)].
  findVarFace(Lc,Nm,Env) where vrEntry(_,Mk,Tp,Fc) ^=isVar(Nm,Env) =>
    some((Mk(Lc,Env),(F^=Fc ? F || faceOfType(Tp,Env)))).
  findVarFace(_,_,_) default => .none.
    
  public varDefined:(string,dict) => boolean.
  varDefined(Nm,Dict) where _ ^= isVar(Nm,Dict) => .true.
  varDefined(_,_) default => .false.

  public varType:(string,dict) => option[tipe].
  varType(Nm,Dict) where vrEntry(_,_,Tp,_) ^= isVar(Nm,Dict) => some(Tp).
  varType(_,_) default => .none.

  public declareVar:(string,option[locn],tipe,option[tipe],dict) => dict.
  declareVar(Nm,Lc,Tp,Fc,Dict) =>
    declareVr(Nm,Lc,Tp,(L,E)=>refreshVar(L,Nm,Tp,E),Fc,Dict).

  public refreshVr:(locn,tipe,dict,(tipe)=>canon) => canon.
  refreshVr(Lc,Tp,Env,Mkr) => valof action{
    (_,VrTp) .= freshen(Tp,Env);
    valis manageConstraints(VrTp,Lc,Mkr(VrTp),Env)
  }    

  public refreshVar(Lc,Nm,Tp,Env) =>
    refreshVr(Lc,Tp,Env,(T)=>vr(Lc,Nm,T)).

  public declareFldAccess:(canon,string,option[locn],tipe,dict) => dict.
  declareFldAccess(Rc,Nm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E) => refreshVr(L,Tp,E,(T)=>dot(L,Rc,Nm,T)),.none,Env).

  public undeclareVar:(string,dict) => dict.
  undeclareVar(_,[]) => [].
  undeclareVar(Nm,[scope(Tps,Vrs,Cns,Imps),..Ev]) =>
    (_ ^= Vrs[Nm] ?
	[scope(Tps,Vrs[~Nm],Cns,Imps),..Ev] ||
	[scope(Tps,Vrs,Cns,Imps),..undeclareVar(Nm,Ev)]).

  public declareConstructor:(string,string,option[locn],tipe,dict) => dict.
  declareConstructor(Nm,FullNm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupEnum(L,FullNm,Tp,Env),.none,Env).

  pickupEnum(Lc,Nm,Tp,Env) => valof action{
--    logMsg("freshen $(Nm)'s type: $(Tp)");
    (_,VrTp) .= freshen(Tp,Env);
--    logMsg("freshened type of $(Nm) is $(VrTp)");
    valis manageConstraints(VrTp,Lc,enm(Lc,Nm,VrTp),Env)
  }

  public declareEnum:(string,string,option[locn],tipe,dict) => dict.
  declareEnum(Nm,FullNm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupEnum(L,FullNm,netEnumType(Tp),Env),.none,Env).

  public mergeDict:(dict,dict,dict) => dict.
  mergeDict(D1,D2,Env) => let{.
    mergeScopes([scope(T1,V1,C1,I1),..Rst],
      [scope(_,V2,_,_),.._]) =>
      [scope(T1,mergeVDefs(V1,V2),C1,I1),..Rst].
    mergeVDefs(V1,V2) => {Nm->E1|Nm->E1 in V1 && E2^=V2[Nm] && sameDesc(E1,E2)}.
    sameDesc(vrEntry(_,_,T1,_),vrEntry(_,_,T2,_)) => sameType(T1,T2,Env)
  .} in mergeScopes(D1,D2).
  
  public declareVr:(string,option[locn],tipe,(locn,dict)=>canon,option[tipe],dict) => dict.
  declareVr(Nm,Lc,Tp,MkVr,Fc,[scope(Tps,Vrs,Cns,Imps),..Ev]) =>
    [scope(Tps,Vrs[Nm->vrEntry(Lc,MkVr,Tp,Fc)],Cns,Imps),..Ev].

  public declareContract:(locn,string,tipe,dict) => dict.
  declareContract(Lc,Nm,Con,[scope(Tps,Vrs,Cns,Imps),..Rest]) =>
    declareMethods(Lc,Con,[scope(Tps[Nm->tpDefn(some(Lc),Nm,typeKey(Con),Con)],Vrs,Cns[Nm->Con],Imps),..Rest]).

  declareMethods:(locn,tipe,dict) => dict.
  declareMethods(Lc,Spec,Dict) where
      (MQ,MI) .= deQuant(Spec) &&
      (MC,typeExists(CT,faceType(Methods,[]))) .= deConstrain(MI) =>
    formMethods(Methods,some(Lc),MQ,MC,CT,Dict).

  formMethods:(cons[(string,tipe)],option[locn],cons[tipe],cons[constraint],tipe,dict) => dict.
  formMethods([],_,_,_,_,Dict) => Dict.
  formMethods([(Nm,Tp),..Mtds],Lc,Q,Cx,Con,Dict) => valof action{
--    logMsg("raw method type of $(Nm) is $(Tp), constraints: $(Con)");
    (MQ,MI) .= deQuant(Tp);
    MT .= reQuant(Q++MQ,reConstrainType([conTract(Con),..Cx],MI));
--    logMsg("actual method type $(MT)");
    valis formMethods(Mtds,Lc,Q,Cx,Con,declareMethod(Nm,Lc,MT,Con,Dict))
  }

  declareMethod:(string,option[locn],tipe,tipe,dict) => dict.
  declareMethod(Nm,Lc,Tp,Con,Dict) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupMtd(L,Nm,Tp,Con,E),.none,Dict).

  pickupMtd(Lc,Nm,Tp,Con,Env) => valof action{
--    logMsg("pick up method $(Nm) : $(Tp) {$(Con)}");
    (Q,VrTp) .= freshen(Tp,Env);
    Cn .= refresh(Q,Con,Env);
--    logMsg("freshened type of $(Nm) is $(VrTp) Q=$(Q)");
--    logMsg("freshened contract $(Cn)");
    valis manageConstraints(VrTp,Lc,mtd(Lc,Nm,Cn,VrTp),Env)
  }

  public pushSig:(tipe,locn,(string,tipe,dict)=>dict,dict) => dict.
  pushSig(faceType(Vrs,Tps),Lc,Mkr,Env) =>
    pushTypes(Tps,Lc,pushFlds(Vrs,Lc,Mkr,Env)).
  
  public pushFace:(tipe,locn,dict) => dict.
  pushFace(Tp,Lc,Env) =>
    pushSig(Tp,Lc,(Id,T,E) where (_,DQ).=deQuant(T) => (_ ^= isConsType(DQ) ?
	  declareConstructor(Id,Id,some(Lc),T,E) ||
	  declareVar(Id,some(Lc),T,.none,E)),
      Env).
  
  pushFlds:(cons[(string,tipe)],locn,(string,tipe,dict)=>dict,dict) => dict.
  pushFlds([],Lc,_,Env) => Env.
  pushFlds([(Nm,Tp),..Vrs],Lc,Mkr,Env)  =>
    pushFlds(Vrs,Lc,Mkr,Mkr(Nm,Tp,Env)).

  pushTypes:(cons[(string,tipe)],locn,dict) => dict.
  pushTypes([],Lc,Env) => Env.
  pushTypes([(Nm,Tp),..Tps],Lc,Env) =>
    pushTypes(Tps,Lc,declareType(Nm,some(Lc),typeKey(Tp),Tp,Env)).

  public declareConstraints:(locn,cons[constraint],dict) => dict.
  declareConstraints(_,[],E) => E.
  declareConstraints(Lc,[contractConstraint(Con),..Cx],Env) where ConNm.=implementationName(Con) =>
    declareConstraints(Lc,Cx,
      declareVar(ConNm,some(Lc),Con,.none,
	declareImplementation(implementationName(Con),Con,Env))).
  declareConstraints(Lc,[_,..Cx],Env) =>
    declareConstraints(Lc,Cx,Env).

  manageConstraints:(tipe,locn,canon,dict) => canon.
  manageConstraints(constrainedType(Tp,Con),Lc,Term,Env) =>
    applyConstraint(Lc,Con,manageConstraints(deRef(Tp),Lc,Term,Env),Env).
  manageConstraints(_,_,Term,Env) => Term.

  applyConstraint:(locn,constraint,canon,dict) => canon.
  applyConstraint(Lc,fieldConstraint(V,F,T),Trm,Env)
      where sameType(typeOf(Trm),V,Env) => overaccess(Lc,Trm,F,T).
  applyConstraint(Lc,conTract(Nm,Args,Deps),Trm,_) =>
    over(Lc,Trm,conTract(Nm,Args,Deps)).
}
