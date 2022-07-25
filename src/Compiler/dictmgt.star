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
  isVar(Nm,[Sc,.._]) where Entry^=Sc.vars[Nm] => some(Entry).
  isVar(Nm,[_,..D]) => isVar(Nm,D).

  public showVar:(string,dict) => string.
  showVar(Nm,Dict) where vrEntry(_,_,Tp,_)^=isVar(Nm,Dict) => "$(Nm)\:$(Tp)".
  showVar(Nm,_) => "$(Nm) not defined".

  public findVar:(option[locn],string,dict) => option[canon].
  findVar(Lc,Nm,Dict) where vrEntry(_,Mk,Tp,_) ^= isVar(Nm,Dict) => some(Mk(Lc,Dict)).
  findVar(_,_,_) default => .none.

  public findImplementation:(dict,string) => option[canon].
  findImplementation(Dict,Nm) => findImpl(Dict,Dict,Nm).
  
  findImpl([Sc,.._],Env,INm) where implEntry(Lc,Vr,Tp) ^= Sc.impls[INm] => some(refreshVar(Lc,Vr,Tp,Env)).
  findImpl([_,..Rest],Env,INm) => findImpl(Rest,Env,INm).
  findImpl([],_,_) => .none.

  public findAccess:(option[locn],tipe,string,dict) => option[canon].
  findAccess(Lc,Tp,Fld,Env) => valof{
    if accEntry(_,Nm,T) ^= getFieldAccess(Tp,Fld,Env) then{
      valis some(refreshVar(Lc,Nm,T,Env))
    }
    else
      valis .none
  }

  public findUpdate:(option[locn],tipe,string,dict) => option[canon].
  findUpdate(Lc,Tp,Fld,Env) => valof{
    if accEntry(_,Nm,T) ^= getFieldUpdate(Tp,Fld,Env) then{
      valis some(refreshVar(Lc,Nm,T,Env))
    }
    else
      valis .none
  }

  public findVarFace:(string,dict) => option[tipe].
  findVarFace(Nm,Env) where vrEntry(_,Mk,Tp,Fc) ^=isVar(Nm,Env) =>
    (_^=Fc ? Fc || faceOfType(Tp,Env)).
  findVarFace(_,_) default => .none.
    
  public varDefined:(string,dict) => boolean.
  varDefined(Nm,Dict) where _ ^= isVar(Nm,Dict) => .true.
  varDefined(_,_) default => .false.

  public varType:(string,dict) => option[tipe].
  varType(Nm,Dict) where vrEntry(_,_,Tp,_) ^= isVar(Nm,Dict) => some(Tp).
  varType(_,_) default => .none.

  public declareVar:(string,option[locn],tipe,option[tipe],dict) => dict.
  declareVar(Nm,Lc,Tp,Fc,Dict) =>
    declareVr(Nm,Lc,Tp,(L,E)=>refreshVar(L,Nm,Tp,E),Fc,Dict).

  refreshVr:(option[locn],tipe,dict,(option[locn],tipe)=>canon) => canon.
  refreshVr(Lc,Tp,Env,Mkr) => valof{
    (_,VrTp) .= freshen(Tp,Env);
    valis manageConstraints(VrTp,Lc,Mkr)
  }    

  public refreshVar(Lc,Nm,Tp,Env) =>
    refreshVr(Lc,Tp,Env,(LLc,T)=>vr(LLc,Nm,T)).

  public declareFldAccess:(canon,string,option[locn],tipe,dict) => dict.
  declareFldAccess(Rc,Nm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E) => refreshVr(L,Tp,E,(LLc,T)=>dot(LLc,Rc,Nm,T)),.none,Env).

  public undeclareVar:(string,dict) => dict.
  undeclareVar(_,[]) => [].
  undeclareVar(Nm,[Sc,..Ev]) =>
    (_ ^= Sc.vars[Nm] ?
	[Sc.vars<<-Sc.vars[~Nm],..Ev] ||
	[Sc,..undeclareVar(Nm,Ev)]).

  public declareConstructor:(string,string,option[locn],tipe,dict) => dict.
  declareConstructor(Nm,FullNm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupEnum(L,FullNm,Tp,Env),.none,Env).

  pickupEnum(Lc,Nm,Tp,Env) => valof{
--    logMsg("freshen $(Nm)'s type: $(Tp)");
    (_,VrTp) .= freshen(Tp,Env);
--    logMsg("freshened type of $(Nm) is $(VrTp)");
    valis manageConstraints(VrTp,Lc,(LLc,ETp)=>enm(LLc,Nm,ETp))
  }

  public declareEnum:(string,string,option[locn],tipe,dict) => dict.
  declareEnum(Nm,FullNm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupEnum(L,FullNm,Tp,E),.none,Env).

  public declareVr:(string,option[locn],tipe,(option[locn],dict)=>canon,option[tipe],dict) => dict.
  declareVr(Nm,Lc,Tp,MkVr,Fc,[Sc,..Ev]) =>
    [Sc.vars<<-Sc.vars[Nm->vrEntry(Lc,MkVr,Tp,Fc)],..Ev].

  public declareContract:(option[locn],string,typeRule,dict) => dict.
  declareContract(Lc,Nm,Con,[Sc,..Rest]) => valof{
    NTps = Sc.types[Nm->tpDefn(Lc,Nm,contractType(Con),contractTypeRule(Con))];
    NCts = Sc.contracts[Nm->Con];
    valis declareMethods(Lc,Con,
      [(Sc.types<<-NTps).contracts<<-NCts,..Rest]).
  }

  declareMethods:(option[locn],typeRule,dict) => dict.
  declareMethods(Lc,Spec,Dict) => valof{
    (Q,contractExists(Nm,Tps,Dps,faceType(Mts,[]))) .= deQuantRule(Spec);
    valis formMethods(Mts,Lc,Q,conTract(Nm,Tps,Dps),Dict)
  }

  formMethods:(cons[(string,tipe)],option[locn],cons[tipe],constraint,dict) => dict.
  formMethods([],_,_,_,Dict) => Dict.
  formMethods([(Nm,Tp),..Mtds],Lc,Q,Con,Dict) => valof{
    (MQ,MI) .= deQuant(Tp);
    (MC,MT) .= deConstrain(MI);
    valis formMethods(Mtds,Lc,Q,Con,
      declareMethod(Nm,Lc,reQuant(Q++MQ,
	  reConstrainType([Con,..MC],MT)),Dict))
  }

  declareMethod:(string,option[locn],tipe,dict) => dict.
  declareMethod(Nm,Lc,Tp,Dict) =>
    declareVr(Nm,Lc,Tp,(L,E)=>pickupMtd(L,Nm,Tp,E),.none,Dict).

  pickupMtd(Lc,Nm,Tp,Env) => valof{
--    logMsg("pick up method $(Nm) : $(Tp) {$(Con)}");
    (Q,VrTp) = freshen(Tp,Env);
--    logMsg("freshened type of $(Nm) is $(VrTp) Q=$(Q)");
--    logMsg("freshened contract $(Cn)");
    valis manageConstraints(VrTp,Lc,(LLc,MTp)=>mtd(LLc,Nm,MTp))
  }

  public mergeDict:(dict,dict,dict) => dict.
  mergeDict(D1,D2,Env) => let{.
    mergeScopes([Sc1,..Rst], [Sc2,.._]) =>
      [Sc1.vars<<-mergeVDefs(Sc1.vars,Sc2.vars),..Rst].

    mergeVDefs:(map[string,vrEntry],map[string,vrEntry])=>map[string,vrEntry].
    mergeVDefs(V1,V2) => {Nm->E1|Nm->E1 in V1 && E2^=V2[Nm] && sameDesc(E1,E2)}.
    sameDesc(vrEntry(_,_,T1,_),vrEntry(_,_,T2,_)) => sameType(T1,T2,Env)

  .} in mergeScopes(D1,D2). 

  public declareDecls:(cons[decl],dict)=>dict.
  declareDecls([],Dict) => Dict.
  declareDecls([D,..Ds],Dict) => 
    declareDecls(Ds,declareDecl(D,Dict)).

  declareDecl(implDec(Lc,Nm,ImplNm,Tp),Dict) => 
    declareImplementation(Lc,Nm,ImplNm,Tp,Dict).
  declareDecl(accDec(Lc,Tp,Fld,AccFn,AccTp),Dict) =>
    declareAccessor(Lc,Tp,Fld,AccFn,AccTp,Dict).
  declareDecl(updDec(Lc,Tp,Fld,AccFn,AccTp),Dict) =>
    declareUpdater(Lc,Tp,Fld,AccFn,AccTp,Dict).
  declareDecl(conDec(Lc,Nm,ConNm,ConRl),Dict) => 
    declareContract(Lc,Nm,ConRl,Dict).
  declareDecl(tpeDec(Lc,Nm,Tp,TpRl),Dict) => 
    declareType(Nm,Lc,Tp,TpRl,Dict).
  declareDecl(varDec(Lc,Nm,FullNm,Tp),Dict) =>
    declareVar(Nm,Lc,Tp,.none,Dict).
  declareDecl(funDec(Lc,Nm,FullNm,Tp),Dict) =>
    declareVar(Nm,Lc,Tp,.none,Dict).
  declareDecl(cnsDec(Lc,Nm,FullNm,Tp),Dict) =>
    declareConstructor(Nm,FullNm,Lc,Tp,Dict).

  public pushSig:(tipe,option[locn],(string,tipe,dict)=>dict,dict) => dict.
  pushSig(faceType(Vrs,Tps),Lc,Mkr,Env) =>
    pushTypes(Tps,Lc,pushFlds(Vrs,Lc,Mkr,Env)).
  
  public pushFace:(tipe,option[locn],dict) => dict.
  pushFace(Tp,Lc,Env) =>
    pushSig(Tp,Lc,(Id,T,E) where (_,DQ).=deQuant(T) => (_ ^= isConsType(DQ) ?
	  declareConstructor(Id,Id,Lc,T,E) ||
	  declareVar(Id,Lc,T,.none,E)),
      Env).
  
  pushFlds:(cons[(string,tipe)],option[locn],(string,tipe,dict)=>dict,dict) => dict.
  pushFlds([],Lc,_,Env) => Env.
  pushFlds([(Nm,Tp),..Vrs],Lc,Mkr,Env)  =>
    pushFlds(Vrs,Lc,Mkr,Mkr(Nm,Tp,Env)).

  pushTypes:(cons[(string,tipe)],option[locn],dict) => dict.
  pushTypes([],Lc,Env) => Env.
  pushTypes([(Nm,Tp),..Tps],Lc,Env) =>
    pushTypes(Tps,Lc,declareType(Nm,Lc,typeKey(Tp),typeExists(Tp,faceType([],[])),Env)).

  public declareConstraints:(option[locn],cons[constraint],dict) => dict.
  declareConstraints(_,[],E) => E.
  declareConstraints(Lc,[conTract(N,T,D),..Cx],Env)
      where ConTp .= mkConType(N,T,D) &&
      ConNm.=implementationName(conTract(N,T,D)) =>
    declareConstraints(Lc,Cx,
      declareVar(ConNm,Lc,ConTp,.none,
	declareImplementation(Lc,ConNm,ConNm,ConTp,Env))).
  declareConstraints(Lc,[_,..Cx],Env) =>
    declareConstraints(Lc,Cx,Env).

  manageConstraints:(tipe,option[locn],(option[locn],tipe)=>canon) => canon.
  manageConstraints(constrainedType(Tp,Con),Lc,Term) =>
    applyConstraint(Lc,Con,manageConstraints(deRef(Tp),Lc,Term)).
  manageConstraints(Tp,Lc,Term) => Term(Lc,Tp).

  applyConstraint:(option[locn],constraint,canon) => canon.
  applyConstraint(Lc,fieldConstraint(V,F,T),Trm) => overaccess(Lc,Trm,V,F,T).
  applyConstraint(Lc,conTract(N,T,D),Trm) =>
    over(Lc,Trm,[conTract(N,T,D)]).
}
