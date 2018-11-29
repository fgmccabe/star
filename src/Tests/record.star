test.record{
  import star.core.

  person ::= someone{name:string. age:integer} | student(string,integer).

  foo:(integer) => {name:string. age:integer}.
  foo(X) => {
    name = "fred".
    age = X.
  }

  fooAge = foo(23).age.

  peteAge = someone{name="fred". age=23}.age.

  nameOf:all k ~~ k<~{name:string} |:(k)=>string.
  nameOf(R) => R.name.

  samsName = nameOf(foo(34)).

  wrap::=wrapped((integer)=>wrap).

  outer(X) => let{
    inner(K) => wrapped((L)=>inner(K)).
  } in inner.

  unwrap:(wrap)=>(integer)=>wrap.
  unwrap(wrapped(X)) => X.

  fooo = unwrap(outer(7)(3)).

  peteName = nameOf({age=45. name="pete"}).
}
