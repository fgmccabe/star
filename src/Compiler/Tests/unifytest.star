test.comp.unify{
  import star.

  import star.compiler.dict.
  import star.compiler.freshen.
  import star.compiler.types.
  import star.compiler.unify.

  funType(A,B) => tpExp(tpExp(tpFun("=>",2),A),B).

  -- A simple generic function type
  T1 = allType(kVar("a"),allType(kVar("b"),
            funType(tupleType([kVar("a"),kVar("b")]),kVar("a")))).

  T2 = funType(tupleType([tipe("integer"),tpExp(tpFun("cons",1),tipe("string"))]),tipe("integer")).

  T1F = freshen(T1,[],stdDict).
  T1V = fst(T1F).
  T1T = snd(T1F).

  assert sameType(T1T,T2,stdDict) && _.= _logmsg(disp(T1T)::string).

  CT1 = allType(kVar("a"),allType(kVar("b"),allType(kVar("c"),
	funType(tupleType([
	      funType(tupleType([kVar("b")]),kVar("c")),
	      funType(tupleType([kVar("a")]),kVar("b"))]),
	  funType(tupleType([kVar("a")]),kVar("c")))))).

  IT1 = allType(kVar("x"),funType(tupleType([kVar("x")]),kVar("x"))).

  CT1F = freshen(CT1,[],stdDict).
  ITF = freshen(IT1,[],stdDict).
  DT = funType(tupleType([tipe("integer")]),tipe("integer")).

  assert V1.=newTypeVar("_") &&
  sameType(funType(tupleType([DT,snd(ITF)]),V1),snd(CT1F),stdDict) &&
  _ .= _logmsg(disp(V1)::string). 
  assert \+ sameType(tipe("integer"),funType(tupleType([]),tipe("string")),stdDict).

  
  
}
