star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.types.

  tpDef ::= tpVar(option[locn],tipe) | tpDefn(option[locn],string,tipe,tipe).

  public vrEntry ::= vrEntry(option[locn],(locn,string,tipe)=>canon,tipe).

  public scope ::= scope(map[string,tpDef],
    map[string,vrEntry],map[string,tipe],
    map[string,map[string,constraint]]).

  public dict ~> cons[scope].

  public implementation display[scope] => let{
    dd(scope(Tps,Vrs,Cons,Impls)) =>
      ss("Types:$(Tps), Vars:$(Vrs), Contracts:$(Cons), Implementations: $(Impls)\n").
  } in {
    disp=dd
  }

  public implementation display[vrEntry] => let{
    dd(vrEntry(Lc,Mk,Tp)) => disp(Tp).
  } in {
    disp(V) => dd(V)
  }

  public implementation display[tpDef] => let{
    dd(tpVar(_,Tp)) => ss("tpvar:$(Tp)").
    dd(tpDefn(_,_,Tmpl,Rl)) => ssSeq([disp(Tmpl),ss("=="),disp(Rl)]).
  } in {
    disp(T) => dd(T)
  }

  public declareVar:(string,option[locn],tipe,dict) => dict.
  declareVar(Nm,Lc,Tp,Dict) =>
    declareVr(Nm,Lc,Tp,(L,Id,T)=>vr(L,Id,T),Dict).

  public declareCon:(string,option[locn],tipe,dict) => dict.
  declareCon(Nm,Lc,Tp,Env) =>
    declareVr(Nm,Lc,Tp,(L,Id,T)=>enm(L,Id,T),Env).

  public declareVr:(string,option[locn],tipe,(locn,string,tipe)=>canon,dict) => dict.
  declareVr(Nm,Lc,Tp,MkVr,[scope(Tps,Vrs,Cns,Imps),..Ev]) =>
    [scope(Tps,Vrs[Nm->vrEntry(Lc,MkVr,Tp)],Cns,Imps),..Ev].

  public isVar:(string,dict) => option[vrEntry].
  isVar(Nm,_) where Tp ^= escapeType(Nm) => some(vrEntry(none,(L,Id,T)=>vr(L,Id,T),Tp)).
  isVar(Nm,[]) => none.
  isVar(Nm,[scope(_,Vrs,_,_),.._]) where Entry^=Vrs[Nm] => some(Entry).
  isVar(Nm,[_,..D]) => isVar(Nm,D).

  public vrType:(vrEntry)=>tipe.
  vrType(vrEntry(_,_,Tp))=>Tp.
  
  public declareType:(string,option[locn],tipe,tipe,dict) => dict.
  declareType(Nm,Lc,Tp,TpRl,[scope(Tps,Vrs,Cns,Imps),..Rest]) =>
    [scope(Tps[Nm->tpDefn(Lc,Nm,Tp,TpRl)],Vrs,Cns,Imps),..Rest].

  public findType:(dict,string) => option[(option[locn],tipe,tipe)].
  findType([],Nm) => none.
  findType([scope(Tps,_,_,_),.._],Ky) where tpDefn(Lc,_,Tp,Rl)^=Tps[Ky] => some((Lc,Tp,Rl)).
  findType([_,..Rest],Ky) => findType(Rest,Ky).

  public declareContract:(locn,string,tipe,dict) => dict.
  declareContract(Lc,Nm,Con,[scope(Tps,Vrs,Cns,Imps),..Rest]) =>
    declareMethods(Lc,Con,[scope(Tps[Nm->tpDefn(some(Lc),Nm,typeKey(Con),Con)],Vrs,Cns[Nm->Con],Imps),..Rest]).

  declareMethods:(locn,tipe,dict) => dict.
  declareMethods(Lc,Spec,Dict) where
      (MQ,MI) .= deQuant(Spec) &&
      (MC,typeExists(CT,faceType(Methods,[]))) .= deConstrain(MI) =>
    formMethods(Methods,some(Lc),MQ,MC,CT,Dict).

  formMethods:(list[(string,tipe)],option[locn],list[tipe],list[constraint],tipe,dict) => dict.
  formMethods([],_,_,_,_,Dict) => Dict.
  formMethods([(Nm,Tp),..Mtds],Lc,Q,Cx,Con,Dict) where
      (MQ,MI) .= deQuant(Tp) &&
      MT .= reConstrain(Cx,constrainedType(Tp,typeConstraint(Con))) =>
    formMethods(Mtds,Lc,Q,Cx,Con,
      declareMethod(Nm,Lc,reQuant(Q++MQ,MT),Dict)).

  public declareMethod:(string,option[locn],tipe,dict) => dict.
  declareMethod(Nm,Lc,Tp,Dict) =>
    declareVr(Nm,Lc,Tp,(L,Id,T)=>mtd(L,Id,T),Dict).
      
  public findContract:(dict,string) => option[tipe].
  findContract([],Nm) => none.
  findContract([scope(_,_,Cns,_),.._],Ky) where Con^=Cns[Ky] => some(Con).
  findContract([_,..Rest],Ky) => findContract(Rest,Ky).

  public findImplementation:(dict,string,string) => option[constraint].
  findImplementation([scope(_,_,_,Imps),.._],Nm,INm) where Ims ^= Imps[Nm] && Imp ^= Ims[INm] => some(Imp).
  findImplementation([_,..Rest],Nm,INm) => findImplementation(Rest,Nm,INm).
  findImplementation([],_,_) => none.

  public declareImplementation:(string,string,tipe,dict) => dict.
  declareImplementation(Nm,ImplNm,Con,[scope(Tps,Vrs,Cns,Imps),..Env]) =>
    (Ims ^= Imps[Nm] ?
	[scope(Tps,Vrs,Cns,Imps[Nm->Ims[ImplNm->typeConstraint(Con)]]),..Env] ||
	[scope(Tps,Vrs,Cns,Imps[Nm->[ImplNm->typeConstraint(Con)]]),..Env]).

  public pushScope:(dict)=>dict.
  pushScope(Env) => [scope([],[],[],[]),..Env].

  public pushSig:(tipe,locn,(locn,string,tipe)=>canon,dict) => dict.
  pushSig(faceType(Vrs,Tps),Lc,Mkr,Env) =>
    pushTypes(Tps,Lc,pushFlds(Vrs,Lc,Mkr,Env)).
  
  public pushFace:(tipe,locn,dict) => dict.
  pushFace(Tp,Lc,Env) =>
    pushSig(deRef(Tp),Lc,(L,Id,T)=>vr(L,Id,T),Env).
  
  pushFlds:(list[(string,tipe)],locn,(locn,string,tipe)=>canon,dict) => dict.
  pushFlds([],Lc,_,Env) => Env.
  pushFlds([(Nm,Tp),..Vrs],Lc,Mkr,Env) =>
    pushFlds(Vrs,Lc,Mkr,declareVr(Nm,some(Lc),Tp,Mkr,Env)).

  pushTypes:(list[(string,tipe)],locn,dict) => dict.
  pushTypes([],Lc,Env) => Env.
  pushTypes([(Nm,Tp),..Tps],Lc,Env) =>
    pushTypes(Tps,Lc,declareType(Nm,some(Lc),typeKey(Tp),Tp,Env)).

  public declareTypeVars:(cons[(string,tipe)],dict) => dict.
  declareTypeVars([],Env) => Env.
  declareTypeVars([(Nm,Tp),..Q],Env) =>
    declareTypeVars(Q,declareType(Nm,none,Tp,Tp,Env)).

  public declareConstraints:(list[constraint],dict) => dict.
  declareConstraints([],E) => E.
  declareConstraints([typeConstraint(Con),..Cx],Env) =>
    declareConstraints(Cx,
      declareImplementation(typeName(Con),implementationName(Con),Con,Env)).
  declareConstraints([_,..Cx],Env) =>
    declareConstraints(Cx,Env).

  emptyFace = faceType([],[]).

-- Standard types are predefined by the language
  public stdDict:dict.
  stdDict =
    declareType("integer",none,intType,typeExists(intType,emptyFace),
      declareType("float",none,fltType,typeExists(fltType,emptyFace),
	declareType("boolean",none,boolType,typeExists(boolType,emptyFace),
	  declareType("string",none,strType,typeExists(strType,emptyFace),
	    declareType("list",none,tpFun("star.core*list",1),
	      allType(kVar("e"),
		typeExists(lstType(kVar("e")),faceType([],[]))),
	      [scope([],[],[],[])]))))).
}
