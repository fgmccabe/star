test.al{
  import star.
  import star.script.

  person[a] ::= someOne{
    name:a.
    age:(integer)=>integer
  }.

  Pete  = someOne{
    name = "peter".
    age(X) => X+30
  }

  main:()=>action[(),()].
  main()=>do{
    assert Pete.name=="peter";
    assert Pete.age(3)==33
  }
}
