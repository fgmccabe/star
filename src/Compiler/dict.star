star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.types.

  tpDef ::= tpVar(option[locn],tipe) | tpDefn(option[locn],string,tipe,tipe).

  public vrEntry ::= vrEntry(option[locn],(locn,string,tipe)=>canon,tipe).

  public contractDefn ::= conDfn(option[locn],string,string,tipe).

  public implDefn ::= implDfn(option[locn],string,string,tipe).

  public scope ::= scope(map[string,tpDef],
    map[string,vrEntry],map[string,contractDefn],
    map[string,map[string,constraint]]).

  public dict ~> cons[scope].

  public declareVar:(string,option[locn],tipe,dict) => dict.
  declareVar(Nm,Lc,Tp,Dict) =>
    declareVr(Nm,Lc,Tp,(L,Id,T)=>vr(L,Id,T),Dict).

  declareVr:(string,option[locn],tipe,(locn,string,tipe)=>canon,dict) => dict.
  declareVr(Nm,Lc,Tp,MkVr,[scope(Tps,Vrs,Cns,Imps),..Ev]) =>
    [scope(Tps,Vrs[Nm->vrEntry(Lc,MkVr,Tp)],Cns,Imps),..Ev].

  public isVar:(string,dict) => option[vrEntry].
  isVar(Nm,_) where Tp ^= escapeType(Nm) => some(vrEntry(none,(L,Id,T)=>vr(L,Id,T),Tp)).
  isVar(Nm,[]) => none.
  isVar(Nm,[scope(_,Vrs,_,_),.._]) where Entry^=Vrs[Nm] => some(Entry).
  isVar(Nm,[_,..D]) => isVar(Nm,D).
  
  public declareType:(string,option[locn],tipe,tipe,dict) => dict.
  declareType(Nm,Lc,Tp,TpRl,[scope(Tps,Vrs,Cns,Imps),..Rest]) =>
    [scope(Tps[Nm->tpDefn(Lc,Nm,Tp,TpRl)],Vrs,Cns,Imps),..Rest].

  public findType:(dict,string) => option[(option[locn],tipe,tipe)].
  findType([],Nm) => none.
  findType([scope(Tps,_,_,_),.._],Ky) where tpDefn(Lc,_,Tp,Rl)^=Tps[Ky] => some((Lc,Tp,Rl)).
  findType([_,..Rest],Ky) => findType(Rest,Ky).

  public declareContract:(string,contractDefn,dict) => dict.
  declareContract(Nm,Con,[scope(Tps,Vrs,Cns,Imps),..Rest]) =>
    declareMethods(Con,[scope(Tps,Vrs,Cns[Nm->Con],Imps),..Rest]).

  declareMethods:(contractDefn,dict) => dict.
  declareMethods(conDfn(Lc,Nm,FullNm,Spec),Dict) where
      (MQ,MI) .= deQuant(Spec) &&
      (MC,contractExists(CT,faceType(Methods,[]))) .= deConstrain(MI) =>
      formMethods(Methods,Lc,MQ,MC,CT,Dict).

  formMethods:(list[(string,tipe)],option[locn],list[tipe],list[constraint],constraint,dict) => dict.
  formMethods([],_,_,_,_,Dict) => Dict.
  formMethods([(Nm,Tp),..Mtds],Lc,Q,Cx,Con,Dict) where
      (MQ,MI) .= deQuant(Tp) &&
      MT .= reConstrain(Cx,constrainedType(Tp,Con)) =>
    formMethods(Mtds,Lc,Q,Cx,Con,
      declareMethod(Nm,Lc,reQuant(Q++MQ,MT),Dict)).

  public declareMethod:(string,option[locn],tipe,dict) => dict.
  declareMethod(Nm,Lc,Tp,Dict) =>
    declareVr(Nm,Lc,Tp,(L,Id,T)=>mtd(L,Id,T),Dict).
      
  public findContract:(dict,string) => option[contractDefn].
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
    pushTypes(Tps,Lc,declareType(Nm,some(Lc),Tp,Tp,Env)).

  public declareTypeVars:(cons[(string,tipe)],dict) => dict.
  declareTypeVars([],Env) => Env.
  declareTypeVars([(Nm,Tp),..Q],Env) => declareTypeVars(Q,declareType(Nm,none,Tp,Tp,Env)).

  public declareConstraints:(list[constraint],dict) => dict.
  declareConstraints([],E) => E.
  declareConstraints([typeConstraint(Con),..Cx],Env) =>
    (conTract(Nm,Args,_) .= Con ?
	declareConstraints(Cx,
	  declareImplementation(Nm,implementationName(Con),Con,Env)) ||
	declareConstraints(Cx,Env)).

-- Standard types are predefined by the language
  public stdDict:dict.
  stdDict =
    declareType("integer",none,intType,intType,
      declareType("float",none,fltType,fltType,
	declareType("boolean",none,boolType,boolType,
	  declareType("string",none,strType,strType,
	    declareType("list",none,tpFun("star.core*list",1),
	      allType(kVar("e"),
		typeExists(lstType(kVar("e")),faceType([],[]))),
	      [scope([],[],[],[])]))))).
}
