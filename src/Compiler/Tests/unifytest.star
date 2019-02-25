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

  T1F = freshen(T1,[],rootDict).
  T1V = fst(T1F).
  T1T = snd(T1F).

  assert sameType(T1T,T2,rootDict) && _.= _logmsg(disp(T1T)::string).
}