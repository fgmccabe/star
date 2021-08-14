test.as{
  import star.
  import star.script.

  pTrm[a,b] ::= pTrm{name:a. age:b}.

  -- peter:{name:string. age:integer}.
  peter = let{
    name = "fred" ++ "'s friend".
  } in pTrm{.name=name. age=23 .}.

  main:()=> action[(),()].
  main() => action{
    show peter.name;

    assert peter.name=="fred's friend" && peter.age==23
  }
}
