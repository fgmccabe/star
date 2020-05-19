test.as{
  import star.
  import star.script.

  -- peter:{name:string. age:integer}.
  peter = let{
    name = "fred" ++ "'s friend".
  } in {.name=name. age=23 .}.

  main:()=>action[(),()].
  main() => do{
    show peter.name;

    assert peter.name=="fred's friend" && peter.age==23
  }
}
