test.assign{
  import star.
  import star.assert.

  person ::= someone{name:ref string. age:integer}.

  rec : (integer)=>person.
  rec(A) => let{
    nm = ref "".
    foo(X) => X+A.
  } in someone{ name=nm. age=foo(10). }.

  fred = rec(24).

  -- peter:{name:string. age:integer}.
  peter = let{.
    name = "fred" ++ "'s friend".
  .} in someone{name= ref name. age=23 }.

  main:() => ().
  main() => valof{
    show fred.name!;
    
    show peter.name!;

    assert valof {
      fred.name := "fred";
      valis fred.name!
    } == "fred";

    show fred.name!;
    
    show peter.name!;

    alpha = ref 23;

    checkInc = (() where _ .= valof {alpha := alpha!+1; valis ()} => alpha!==24);

    assert alpha!==23;

    show alpha!;

    assert checkInc();

    assert peter.name!=="fred's friend" && peter.age==23;
    valis ()
  }
}
