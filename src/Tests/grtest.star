test.gr{
  import star.

  -- Test simple grammars

  expr: (integer) --> list[integer].
  expr(x+y) --> factor(x), [0c+], factor(y).
  expr(x-y) --> factor(x), "-", factor(y).
  expr(x) --> factor(x).

  factor:(integer) --> list[integer].
  factor(x*y) --> term(x), [0c*], term(y).
  factor(x) --> term(x).

  term:(integer) --> list[integer].
  term(x) --> [0c(], expr(x), [0c)].
  term(x) --> num(x).

  num:(integer) --> list[integer].
  num(0) --> [0c0].
  num(1) --> [0c1,0c2].

  assert expr(X) .~ _explode("1+2*1") && X==3.
/*
  assert 3+3=.foo(X).

  assert  expr(_explode("1+2"))=.some(([],X)) && X==3

  foo:(xxx)<=integer.

  expr : (integer) --> list[integer].
  expr(x+y) given (S) --> factor(x) given (S+1), [0c+], factor(y) given (S+x+2), +NT.
  expr(x-y) --> factor(x), "-", factor(y).
  expr(x) --> factor(x).
  */
}
