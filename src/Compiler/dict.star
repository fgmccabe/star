star.compiler.dict{
  import star.

  import star.compiler.canon.
  import star.compiler.escapes.
  import star.compiler.location.
  import star.compiler.types.

  tpDef ::= tpVar(option[locn],tipe) |
  tpDefn(option[locn],string,tipe,tipe).

  public vrEntry ::= vrEntry(option[locn],(locn,tipe)=>canon,tipe,()=>tipe).

  public contractDefn ::= conDfn(option[locn],string,list[tipe],list[tipe],list[tipe],tipe).

  public scope ::= scope(map[string,tpDef],
    map[string,vrEntry],map[string,contractDefn],
    map[string,map[string,constraint]]).

  public dict ~> cons[scope].

  public declareType:(string,option[locn],tipe,dict) => dict.
  declareType(Nm,Lc,Tp,[scope(Tps,Vrs,Cns,Imps),..Rest]) => [scope(Tps[Nm->tpDefn(Lc,Nm,Tp,voidType)],Vrs,Cns,Imps),..Rest].

  public findType:(dict,string) => option[(option[locn],tipe,tipe)].
  findType([],Nm) => none.
  findType([scope(Tps,_,_,_),.._],Ky) where tpDefn(Lc,_,Tp,Rl)^=Tps[Ky] => some((Lc,Tp,Rl)).
  findType([_,..Rest],Ky) => findType(Rest,Ky).

  public findContract:(dict,string) => option[contractDefn].
  findContract([],Nm) => none.
  findContract([scope(_,_,Cns,_),.._],Ky) where Con^=Cns[Ky] => some(Con).
  findContract([_,..Rest],Ky) => findContract(Rest,Ky).

  public findImplementation:(dict,string,string) => option[constraint].
  findImplementation([scope(_,_,_,Imps),.._],Nm,INm) where Ims ^= Imps[Nm] && Imp ^= Ims[INm] => some(Imp).
  findImplementation([_,..Rest],Nm,INm) => findImplementation(Rest,Nm,INm).
  findImplementation([],_,_) => none.

-- Standard types are predefined by the language
  public stdDict:dict.
  stdDict = [
    scope(["integer"->tpDefn(none,"integer",
	  tipe("star.core*integer"),
	  tipe("star.core*integer")),
	"float" -> tpDefn(none,"float",
	  tipe("star.core*float"),tipe("star.core*float")),
	"boolean" -> tpDefn(none,"boolean",
	  tipe("star.core*boolean"),tipe("star.core*boolean")),
	"list" -> tpDefn(none,"list",tpFun("star.core*list",1),
	  allType(kVar("e"),
	    typeExists(tpExp(tpFun("star.core*list",1),kVar("e")),
	      faceType([],[]))))
      ],[],[],[])].

}
