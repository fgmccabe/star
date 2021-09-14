test.assign{
  import star.
  import star.script.

  person ::= someone{name:ref string. age:integer} | student(string,integer).

  rec : (integer)=>person.
  rec(A) => let{.
    nm = ref "".
    foo(X) => X+A.
  .} in someone{. name=nm. age=foo(10). .}.

  fred = rec(24).

  -- peter:{name:string. age:integer}.
  peter = let{
    name = "fred" ++ "'s friend".
  } in someone{.name= ref name. age=23 .}.

  main:() => action[(),()].
  main() => action{
    assert valof action {
      fred.name := "fred";
      valis fred.name!
    } == "fred";

    show fred.name!;
    
    show peter.name!;

    alpha .= ref 23;

    checkInc .= (() where _ .= valof action {alpha := alpha!+1; valis ()} => alpha!==24);

    assert alpha!==23;

    show alpha!;

    assert checkInc();

    assert peter.name!=="fred's friend" && peter.age==23
  }
}
