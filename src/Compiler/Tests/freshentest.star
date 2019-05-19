test.comp.freshen{
  import star.

  import star.compiler.dict.
  import star.compiler.freshen.
  import star.compiler.types.

  -- A simple generic function type
  T1 = allType(kVar("a"),allType(kVar("b"),
            funType(tupleType([kVar("a"),kVar("b")]),kVar("a")))).

  show disp(T1).

  show disp(freshen(T1,[],stdDict)).

  T2 = allType(kVar("b"),
            funType(tupleType([allType(kVar("a"),funType(tupleType([kVar("a"),kVar("b")]),kVar("a"))),kVar("b")]),kVar("b"))).

  show disp(freshen(T2,[],stdDict)).

  T3 = allType(kVar("a"),tupleType([kVar("a"),kVar("a")])).

  F1 = snd(freshen(T3,[],stdDict)).

  show disp(F1).

  assert tupleType([V1,V2]).=F1 && V1==V2 && isUnbound(V1).

  S1 = snd(evidence(T3,[],stdDict)).

  show disp(S1).

  assert tupleType([V1,V2]).=S1 && V1==V2.
}
