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

  isVar:(string,dict) => option[vrEntry].
  isVar(Nm,_) where (Tp,_) ^= intrinsic(Nm) =>
    some(vrEntry(.none,(L,E)=>refreshVar(L,Nm,Tp,E),Tp)).
  isVar(Nm,_) where Tp ^= escapeType(Nm) => some(vrEntry(.none,(L,E)=>refreshVar(L,Nm,Tp,E),Tp)).
  isVar(Nm,[]) => .none.
  isVar(Nm,[scope(_,Vrs,_,_),.._]) where Entry^=Vrs[Nm] => some(Entry).
  isVar(Nm,[_,..D]) => isVar(Nm,D).

  public findVar:(locn,string,dict) => option[canon].
  findVar(Lc,Nm,Dict) where vrEntry(_,Mk,Tp) ^= isVar(Nm,Dict) => some(Mk(Lc,Dict)).
  findVar(_,_,_) default => .none.

  public varDefined:(string,dict) => boolean.
  varDefined(Nm,Dict) where _ ^= isVar(Nm,Dict) => .true.
  varDefined(_,_) default => .false.

  public varType:(string,dict) => option[tipe].
  varType(Nm,Dict) where vrEntry(_,_,Tp) ^= isVar(Nm,Dict) => some(Tp).
  varType(_,_) default => .none.

  public declareVar:(string,option[locn],tipe,dict) => dict.
  declareVar(Nm,Lc,Tp,Dict) =>
    declareVr(Nm,Lc,Tp,(L,E)=>refreshVar(L,Nm,Tp,E),Dict).

  public refreshVr:(locn,tipe,dict,(tipe)=>canon) => canon.
  refreshVr(Lc,Tp,Env,Mkr) => valof action{
    (_,VrTp) .= freshen(Tp,Env);
    valis manageConstraints(VrTp,[],Lc,Mkr(VrTp),Env)
  }    

  public refreshVar(Lc,Nm,Tp,Env) =>
    refreshVr(Lc,Tp,Env,(T)=>vr(Lc,Nm,T)).

  public declareFldAccess:(canon,string,option[locn],tipe,dict) => dict.
  declareFldAccess(Rc,Nm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E) => refreshVr(L,Tp,E,(T)=>dot(L,Rc,Nm,T)),Env).
  
  public undeclareVar:(string,dict) => dict.
  undeclareVar(_,[]) => [].
  undeclareVar(Nm,[scope(Tps,Vrs,Cns,Imps),..Ev]) =>
    (_ ^= Vrs[Nm] ?
	[scope(Tps,Vrs[!Nm],Cns,Imps),..Ev] ||
	[scope(Tps,Vrs,Cns,Imps),..undeclareVar(Nm,Ev)]).

  public declareConstructor:(string,string,option[locn],tipe,dict) => dict.
  declareConstructor(Nm,FullNm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupEnum(L,FullNm,Tp,Env),Env).

  pickupEnum(Lc,Nm,Tp,Env) => valof action{
    (_,VrTp) .= freshen(Tp,Env);
--    logMsg("freshened type of $(Nm) is $(VrTp)");
    valis manageConstraints(VrTp,[],Lc,enm(Lc,Nm,VrTp),Env)
  }

  public declareEnum:(string,string,option[locn],tipe,dict) => dict.
  declareEnum(Nm,FullNm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupEnum(L,FullNm,netEnumType(Tp),Env),Env).
  
  public declareVr:(string,option[locn],tipe,(locn,dict)=>canon,dict) => dict.
  declareVr(Nm,Lc,Tp,MkVr,[scope(Tps,Vrs,Cns,Imps),..Ev]) =>
    [scope(Tps,Vrs[Nm->vrEntry(Lc,MkVr,Tp)],Cns,Imps),..Ev].

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
    MT .= reQuant(Q++MQ,reConstrainType([contractConstraint(Con),..Cx],MI));
--    logMsg("actual method type $(MT)");
    valis formMethods(Mtds,Lc,Q,Cx,Con,declareMethod(Nm,Lc,MT,Con,Dict))
  }

  declareMethod:(string,option[locn],tipe,tipe,dict) => dict.
  declareMethod(Nm,Lc,Tp,Con,Dict) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupMtd(L,Nm,Tp,Con,E),Dict).

  pickupMtd(Lc,Nm,Tp,Con,Env) => valof action{
--    logMsg("pick up method $(Nm) : $(Tp) {$(Con)}");
    (Q,VrTp) .= freshen(Tp,Env);
    Cn .= refresh(Q,Con,Env);
--    logMsg("freshened type of $(Nm) is $(VrTp) Q=$(Q)");
--    logMsg("freshened contract $(Cn)");
    valis manageConstraints(VrTp,[],Lc,mtd(Lc,Nm,Cn,VrTp),Env)
  }
    
  public pushSig:(tipe,locn,(string,tipe,dict)=>dict,dict) => dict.
  pushSig(faceType(Vrs,Tps),Lc,Mkr,Env) =>
    pushTypes(Tps,Lc,pushFlds(Vrs,Lc,Mkr,Env)).
  
  public pushFace:(tipe,locn,dict) => dict.
  pushFace(Tp,Lc,Env) =>
    pushSig(deRef(Tp),Lc,(Id,T,E) where (_,DQ).=deQuant(T) => (_ ^= isConsType(DQ) ?
	  declareConstructor(Id,Id,some(Lc),T,E) ||
	  declareVar(Id,some(Lc),T,E)),Env).
  
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
      declareVar(ConNm,some(Lc),Con,
	declareImplementation(implementationName(Con),Con,Env))).
  declareConstraints(Lc,[_,..Cx],Env) =>
    declareConstraints(Lc,Cx,Env).

  manageConstraints:(tipe,cons[constraint],locn,canon,dict) => canon.
  manageConstraints(constrainedType(Tp,Con),Cons,Lc,Term,Env)
      where C0 .= applyConstraint(Con,Cons) =>
    manageConstraints(deRef(Tp),C0,Lc,Term,Env).
  manageConstraints(_,[],_,Term,Env) => Term.
  manageConstraints(Tp,Cons,Lc,Term,Env) => over(Lc,Term,Tp,reverse(Cons)).

  applyConstraint:(constraint,cons[constraint]) => cons[constraint].
  applyConstraint(fieldConstraint(T,F),Cons) => valof do{
    _ <- addConstraint(T,fieldConstraint(T,F));
    valis Cons
  }.
  applyConstraint(Con,Cons) where contractConstraint(A).=Con => valof do{
    AA := deRef(A);
    while tpExp(Op,Arg) .= AA!! do{
      _ <- addConstraint(Arg,Con);
      AA := deRef(Op)
    };
    valis [Con,..Cons]
  }



}
