star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.escapes.
  import star.compiler.intrinsics.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.types.

  public tpDef ::= tpVar(option[locn],tipe) | tpDefn(option[locn],string,tipe,tipe).

  public vrEntry ::= vrEntry(option[locn],(locn,dict)=>canon,tipe,option[tipe]).

  public scope ::= scope(map[string,tpDef],
    map[string,vrEntry],map[string,tipe],
    map[string,constraint]).

  public dict ~> cons[scope].

  public implementation display[scope] => {
    disp(scope(Tps,Vrs,Cons,Impls)) =>
      "Types:$(Tps),\nVars:$(Vrs),\nContracts:$(Cons),\nImplementations: $(Impls)\n".
  }

  public implementation display[vrEntry] => {
    disp(vrEntry(some(Lc),Mk,Tp,_)) => "$(Mk(Lc,emptyDict))|=$(Tp)".
    disp(vrEntry(.none,_,Tp,_)) => disp(Tp).
  }

  public implementation display[tpDef] => {
    disp(tpVar(_,Tp)) => "tpvar:$(Tp)".
    disp(tpDefn(_,_,Tmpl,Rl)) => "$(Tmpl) == $(Rl)".
  } 

  public vrType:(vrEntry)=>tipe.
  vrType(vrEntry(_,_,Tp,_))=>Tp.

  public vrFace(vrEntry(_,_,_,Fc))=>Fc.
  
  public declareType:(string,option[locn],tipe,tipe,dict) => dict.
  declareType(Nm,Lc,Tp,TpRl,[scope(Tps,Vrs,Cns,Imps),..Rest]) =>
    [scope(Tps[Nm->tpDefn(Lc,Nm,Tp,TpRl)],Vrs,Cns,Imps),..Rest].

  public findType:(dict,string) => option[(option[locn],tipe,tipe)].
  findType([],Nm) => .none.
  findType([scope(Tps,_,_,_),.._],Ky) where tpDefn(Lc,_,Tp,Rl)^=Tps[Ky] => some((Lc,Tp,Rl)).
  findType([_,..Rest],Ky) => findType(Rest,Ky).

  public findContract:(dict,string) => option[tipe].
  findContract([],Nm) => .none.
  findContract([scope(_,_,Cns,_),.._],Ky) where Con^=Cns[Ky] => some(Con).
  findContract([_,..Rest],Ky) => findContract(Rest,Ky).

  public findImplementation:(dict,string) => option[constraint].
  findImplementation([scope(_,_,_,Imps),.._],INm) where Imp ^= Imps[INm] => some(Imp).
  findImplementation([_,..Rest],INm) => findImplementation(Rest,INm).
  findImplementation([],_) => .none.

  public declareImplementation:(string,constraint,dict) => dict.
  declareImplementation(ImplNm,Con,[scope(Tps,Vrs,Cns,Imps),..Env]) =>
    [scope(Tps,Vrs,Cns,Imps[ImplNm->Con]),..Env].

  public undeclareImplementation:(string,dict) => dict.
  undeclareImplementation(Nm,[scope(Tps,Vrs,Cns,Imps),..Env]) =>
    [scope(Tps,Vrs,Cns,Imps[~Nm]),..Env].

  public pushScope:(dict)=>dict.
  pushScope(Env) => [scope([],[],[],[]),..Env].

  public declareTypeVars:(cons[(string,tipe)],dict) => dict.
  declareTypeVars([],Env) => Env.
  declareTypeVars([(Nm,Tp),..Q],Env) =>
    declareTypeVars(Q,declareType(Nm,.none,Tp,Tp,Env)).

  emptyFace = faceType([],[]).

  public emptyDict:dict.
  public emptyDict = [scope([],[],[],[])].

-- Standard types are predefined by the language
  public stdDict:dict.
  stdDict =
    declareType("integer",.none,intType,typeExists(intType,emptyFace),
      declareType("float",.none,fltType,typeExists(fltType,emptyFace),
	declareType("boolean",.none,boolType,typeExists(boolType,emptyFace),
	  declareType("string",.none,strType,typeExists(strType,emptyFace),
	    declareType("cons",.none,tpFun("star.core*cons",1),
	      allType(nomnal("e"),
		typeExists(lstType(nomnal("e")),faceType([],[]))),
	      emptyDict))))).
}
