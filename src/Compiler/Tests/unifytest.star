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

  show disp(T1).

}
