star.compiler.dict.mgt{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.freshen.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.unify.

  isVar:(string,dict) => option[vrEntry].
  isVar(Nm,Env) => valof{
    if Tp ?= escapeType(Nm) then
      valis .some(.vrEntry(.none,(L,R,E)=>refreshVar(L,Nm,R,Tp,E),Tp,.none))
    else
    valis dictVar(Nm,Env)
  }

  dictVar:(string,dict) => option[vrEntry].
  dictVar(Nm,.dict(Scs,_)) => let{.
    dictV([]) => .none.
    dictV([Sc,.._]) where Entry?=Sc.vars[Nm] => .some(Entry).
    dictV([_,..Env]) => dictV(Env).
  .} in dictV(Scs).
	  
  public showVar:(string,dict) => string.
  showVar(Nm,Dict) where .vrEntry(_,_,Tp,_)?=isVar(Nm,Dict) => "$(Nm)\:$(Tp)".
  showVar(Nm,_) => "$(Nm) not defined".

  public findVar:(option[locn],string,boolean,dict) => option[canon].
  findVar(Lc,Nm,Refresh,Dict) where .vrEntry(_,Mk,Tp,_) ?= isVar(Nm,Dict) =>
    .some(Mk(Lc,Refresh,Dict)).
  findVar(_,_,_,_) default => .none.

  public findImplementation:(dict,string) => option[canon].
  findImplementation(.dict(Scs,Br),Nm) => let{.
    findImpl([Sc,.._]) where .implEntry(Lc,Vr,Tp) ?= Sc.impls[Nm] =>
      .some(refreshVar(Lc,Vr,.true,Tp,.dict(Scs,Br))).
    findImpl([_,..Rest]) => findImpl(Rest).
    findImpl([]) => .none.
  .} in findImpl(Scs).
  

  public findAccess:(option[locn],tipe,string,dict) => option[canon].
  findAccess(Lc,Tp,Fld,Env) => valof{
    if .accEntry(_,Nm,T) ?= getFieldAccess(Tp,Fld,Env) then{
      valis .some(refreshVar(Lc,Nm,.true,T,Env))
    }
    else
      valis .none
  }

  public findUpdate:(option[locn],tipe,string,dict) => option[canon].
  findUpdate(Lc,Tp,Fld,Env) => valof{
    if .accEntry(_,Nm,T) ?= getFieldUpdate(Tp,Fld,Env) then{
      valis .some(refreshVar(Lc,Nm,.true,T,Env))
    }
    else
      valis .none
  }

  public findVarFace:(string,dict) => option[tipe].
  findVarFace(Nm,Env) where .vrEntry(_,_,Tp,Fc) ?=isVar(Nm,Env) =>
    (_?=Fc ?? Fc || faceOfType(Tp,Env)).
  findVarFace(_,_) default => .none.
    
  public varDefined:(string,dict) => boolean.
  varDefined(Nm,Dict) where _ ?= isVar(Nm,Dict) => .true.
  varDefined(_,_) default => .false.

  public varType:(string,dict) => option[tipe].
  varType(Nm,Dict) where .vrEntry(_,_,Tp,_) ?= isVar(Nm,Dict) => .some(Tp).
  varType(_,_) default => .none.

  public declareVar:(string,string,option[locn],tipe,option[tipe],dict) => dict.
  declareVar(Nm,FullNm,Lc,Tp,Fc,Dict) => valof{
    if isEscape(Nm) then{
      reportError("Cannot redeclare an escape: $(Nm)",Lc);
      valis Dict
    } else
    valis declareVr(Nm,Lc,Tp,(L,R,E)=>refreshVar(L,FullNm,R,Tp,E),Fc,Dict)
  }

  refreshVr:(option[locn],tipe,dict,(option[locn],tipe)=>canon) => canon.
  refreshVr(Lc,Tp,Env,Mkr) => valof{
    (_,VrTp) = freshen(Tp,Env);
    valis manageConstraints(VrTp,Lc,(VTp) => Mkr(Lc,VTp))
  }    

  refreshVar(Lc,Nm,.true,Tp,Env) =>
    refreshVr(Lc,Tp,Env,(LLc,T)=>.vr(LLc,Nm,T)).
  refreshVar(Lc,Nm,.false,Tp,Env) => .vr(Lc,Nm,Tp).

  public declareFldAccess:(canon,string,option[locn],tipe,dict) => dict.
  declareFldAccess(Rc,Nm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,_,E) => refreshVr(L,Tp,E,(LLc,T)=>.dot(LLc,Rc,Nm,T)),.none,Env).

  public undeclareVar:(string,dict) => dict.
  undeclareVar(Nm,.dict(Scs,Br)) => let{.
    undeclareV([]) => [].
    undeclareV([Sc,..Ev]) =>
      (_ ?= Sc.vars[Nm] ??
	[Sc.vars=Sc.vars[~Nm],..Ev] ||
	[Sc,..undeclareV(Ev)])
  .} in .dict(undeclareV(Scs),Br).

  public declareConstructor:(string,string,option[locn],tipe,dict) => dict.
  declareConstructor(Nm,FullNm,Lc,Tp,Env) => valof{
    AbtTpNm = localName(tpName(funTypeRes(Tp)),.typeMark);
    valis declareCns(Lc,FullNm,Tp,AbtTpNm,
      declareVr(Nm,Lc,Tp,(L,R,E)=>pickupEnum(L,Nm,R,Tp,E),.none,Env)).
  }

  declareCns(CLc,Nm,Tp,TpNm,.dict(Scs,Br)) => let{.
    declCns([Level,..Rest]) where .tpDefn(Lc,TNm,TTp,TpRl,Cons)?=Level.types[TpNm] =>
      [Level.types=Level.types[TpNm->.tpDefn(Lc,TNm,TTp,TpRl,Cons[Nm->Tp])],..Rest].
    declCns([Level,..Rest]) => [Level,..declCns(Rest)].
    declCns([]) => valof{
      reportError("cannot declare constructor #(Nm)\:$(Tp), could not find type $(TpNm)",CLc);
      valis Scs
    }
  .} in .dict(declCns(Scs),Br).

  public findConstructors:(tipe,dict)=>option[map[string,tipe]].
  findConstructors(Tp,Dict) where (_,_,_,Mp) ?=
    findType(Dict,localName(tpName(Tp),.typeMark)) => .some(Mp).
  findConstructors(_,_) default => .none.

  pickupEnum(Lc,Nm,.true,Tp,Env) => valof{
    (_,VrTp) = freshen(Tp,Env);
    valis manageConstraints(VrTp,Lc,(ETp)=>.enm(Lc,Nm,ETp))
  }
  pickupEnum(Lc,Nm,.false,Tp,Env) => .enm(Lc,Nm,Tp).

  public declareEnum:(string,string,option[locn],tipe,dict) => dict.
  declareEnum(Nm,FullNm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,R,E)=>pickupEnum(L,FullNm,R,Tp,E),.none,Env).

  public declareVr:(string,option[locn],tipe,(option[locn],boolean,dict)=>canon,option[tipe],dict) => dict.
  declareVr(Nm,Lc,Tp,MkVr,Fc,.dict([Sc,..Ev],Br)) => valof{
    valis .dict([Sc.vars=Sc.vars[Nm->.vrEntry(Lc,MkVr,Tp,Fc)],..Ev],Br)
  }.

  public declareContract:(option[locn],string,typeRule,dict) => dict.
  declareContract(Lc,Nm,Con,.dict([Sc,..Rest],Br)) => valof{
    NCts = Sc.contracts[Nm->Con];
    valis declareMethods(Lc,Con,.dict([Sc.contracts=NCts,..Rest],Br)).
  }

  declareMethods:(option[locn],typeRule,dict) => dict.
  declareMethods(Lc,Spec,Dict) => valof{
    if (Q,.contractExists(Nm,Tps,Dps,.faceType(Mts,[]))) .= deQuantRule(Spec) then
      valis formMethods(Mts,Lc,Q,.conTract(Nm,Tps,Dps),Dict)
    else{
      reportError("invalid contract spec $(Spec)",Lc);
      valis Dict
    }
  }

  formMethods:(cons[(string,tipe)],option[locn],cons[tipe],constraint,dict) => dict.
  formMethods([],_,_,_,Dct) => Dct.
  formMethods([(Nm,Tp),..Mtds],Lc,Q,Con,Dct) => valof{
    (MQ,MI) = deQuant(Tp);
    (_MC,MT) = deConstrain(MI);   -- We drop the constraint here
    valis formMethods(Mtds,Lc,Q,Con,
      declareMethod(Nm,Lc,reQuant(Q++MQ,.constrainedType(MT,Con)),Dct))
  }

  declareMethod:(string,option[locn],tipe,dict) => dict.
  declareMethod(Nm,Lc,Tp,Dict) =>
    declareVr(Nm,Lc,Tp,(L,R,E)=>pickupMtd(L,Nm,R,Tp,E),.none,Dict).

  pickupMtd(Lc,Nm,.true,Tp,Env) => valof{
    (Q,VrTp) = freshen(Tp,Env);
    valis manageConstraints(VrTp,Lc,(MTp)=>.mtd(Lc,Nm,MTp))
  }
  pickupMtd(Lc,Nm,.false,Tp,_) => .mtd(Lc,Nm,Tp).

  public mergeDict:(dict,dict,dict) => dict.
  mergeDict(.dict(D1,Br),D2,Env) => let{.
    mergeScopes([Sc1,..Rst]) => [Sc1.vars=mergeVDefs(Sc1.vars),..Rst].

    mergeVDefs:(map[string,vrEntry])=>map[string,vrEntry].
    mergeVDefs(V1) => {Nm->E1|Nm->E1 in V1 && E2 ?= dictVar(Nm,D2) && sameDesc(E1,E2)}.

    sameDesc(.vrEntry(_,_,T1,_),.vrEntry(_,_,T2,_)) => sameType(T1,T2,Env)

  .} in .dict(mergeScopes(D1),Br).

  public declareDecls:(cons[decl],dict)=>dict.
  declareDecls([],Dict) => Dict.
  declareDecls([D,..Ds],Dict) => 
    declareDecls(Ds,declareDecl(D,Dict)).

  declareDecl(Dc,Dict) => case Dc in {
    .implDec(Lc,Nm,ImplNm,Tp) => 
      declareImplementation(Lc,Nm,ImplNm,Tp,Dict).
    .accDec(Lc,Tp,Fld,AccFn,AccTp) =>
      declareVar(AccFn,AccFn,Lc,AccTp,.none,declareAccessor(Lc,Tp,Fld,AccFn,AccTp,Dict)).
    .updDec(Lc,Tp,Fld,AccFn,AccTp) =>
      declareVar(AccFn,AccFn,Lc,AccTp,.none,declareUpdater(Lc,Tp,Fld,AccFn,AccTp,Dict)).
    .conDec(Lc,Nm,ConNm,ConRl) => 
      declareContract(Lc,Nm,ConRl,Dict).
    .tpeDec(Lc,Nm,Tp,TpRl) => 
      declareType(Nm,Lc,Tp,TpRl,Dict).
    .varDec(Lc,Nm,FullNm,Tp) =>
      declareVar(Nm,FullNm,Lc,Tp,.none,Dict).
    .funDec(Lc,Nm,FullNm,Tp) =>
      declareVar(Nm,FullNm,Lc,Tp,.none,Dict).
    .cnsDec(Lc,Nm,FullNm,Tp) =>
      declareConstructor(Nm,FullNm,Lc,Tp,Dict).
  }

  public pushFace:(tipe,option[locn],dict,string) => dict.
  pushFace(Tp,Lc,Env,Path) =>
    pushSig(Tp,Lc,(Id,T,E) where (_,DQ).=deQuant(T) => (_ ?= isConsType(DQ) ??
	declareConstructor(Id,qualifiedName(Path,.valMark,Id),Lc,T,E) ||
	declareVar(Id,qualifiedName(Path,.valMark,Id),Lc,T,.none,E)),
      Env).

  pushSig:(tipe,option[locn],(string,tipe,dict)=>dict,dict) => dict.
  pushSig(.faceType(Vrs,Tps),Lc,Mkr,Env) =>
    pushTypes(Tps,Lc,pushFlds(Vrs,Lc,Mkr,Env)).
  
  pushFlds:(cons[(string,tipe)],option[locn],(string,tipe,dict)=>dict,dict) => dict.
  pushFlds([],Lc,_,Env) => Env.
  pushFlds([(Nm,Tp),..Vrs],Lc,Mkr,Env)  =>
    pushFlds(Vrs,Lc,Mkr,Mkr(Nm,Tp,Env)).

  pushTypes:(cons[(string,typeRule)],option[locn],dict) => dict.
  pushTypes([],Lc,Env) => Env.
  pushTypes([(Nm,Rl),..Tps],Lc,Env) =>
    pushTypes(Tps,Lc,declareType(Nm,Lc,ruleType(Rl),Rl,Env)).

  public declareConstraints:(option[locn],cons[constraint],dict) => dict.
  declareConstraints(_,[],E) => E.
  declareConstraints(Lc,[.conTract(N,T,D),..Cx],Env)
      where ConTp .= mkConType(N,T,D) &&
      ConNm.=implementationName(.conTract(N,T,D)) =>
    declareConstraints(Lc,Cx,
      declareVar(ConNm,ConNm,Lc,ConTp,.none,
	declareImplementation(Lc,ConNm,ConNm,ConTp,Env))).
  declareConstraints(Lc,[.implicit(Nm,Tp),..Cx],Env) =>
    declareConstraints(Lc,Cx,declareVar(Nm,Nm,Lc,Tp,faceOfType(Tp,Env),Env)).
  declareConstraints(Lc,[.raisEs(Tp),..Cx],Env) =>
    declareConstraints(Lc,Cx,declareVar("$try","$try",Lc,Tp,faceOfType(Tp,Env),Env)).
  declareConstraints(Lc,[_,..Cx],Env) =>
    declareConstraints(Lc,Cx,Env).

  public manageConstraints:(tipe,option[locn],(tipe)=>canon) => canon.
  manageConstraints(.constrainedType(Tp,Con),Lc,Term) =>
    manageConstraints(deRef(Tp),Lc,(TT)=>applyConstraint(Lc,Con,Term(TT))).
  manageConstraints(Tp,Lc,Term) => Term(Tp).

  applyConstraint:(option[locn],constraint,canon) => canon.
  applyConstraint(Lc,Con,Trm) => .over(Lc,Trm,Con).

  public declareBrType:(string,cons[canonDef],cons[decl],dict) => ().
  declareBrType(Nm,Dfs,Dcs,.dict(Scs,Br)) => valof{
    Br[Nm] := (Dfs,Dcs);
    valis ()
  }

  public declareBrTypes:(dict)=>dict.
  declareBrTypes(.dict(Scs,Br)) => valof{
    D := .dict(Scs,ref []);
    for Nm->(_,Dcs) in Br! do{
      D := declareDecls(Dcs,D!)
    };
    valis D!
  }

  public pullBrDefs:(dict) => (cons[canonDef],cons[decl]).
  pullBrDefs(.dict(_,Br)) => valof{
    D := [];
    Dc := [];
    for Nm->(Dfs,Dcs) in Br! do{
      D := Dfs++D!;
      Dc := Dcs++Dc!;
    };
    valis (D!,Dc!)
  }
}
