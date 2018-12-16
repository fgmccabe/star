test.assign{
  import star.

  person ::= someone{name:ref string. age:integer} | student(string,integer).

  alpha := 23.

  assert alpha!==23.

  checkInc:()=>boolean.
  checkInc() where _ .= (alpha := alpha!+1) => alpha!==24.

  assert checkInc().

  show alpha!.

  rec : (integer)=>person.
  rec(A) => let{
    nm := "".
    foo(X) => X+A.
  } in someone{. name=nm. age=foo(10). .}.

  fred = rec(24).

  assert (fred.name:="fred")=="fred".
  show fred.name! .

  -- peter:{name:string. age:integer}.
  peter = let{
    name = "fred" ++ "'s friend".
  } in {.name=name. age=23 .}.

  show peter.name.

  -- assert peter.name=="fred's friend" && peter.age==23.
}
