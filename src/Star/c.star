star.c {

  public all t ~~ option[t] ::= none | some(t).

  public contract all x ~~ equality[x] ::= {
    (==): (x,x)=>logical.
    hash:(x)=>integer.
  }

  /*
  public (\==):all x ~~ equality[x] |: (x,x)=>logical.
  x \== y => \+ x==y.

  public implementation equality[float] => {
    X == Y => __flt_eq(X,Y).
    hash(X) => _flt_hash(X).
  }

  public (>) : all t ~~ comp[t] |: (t,t)=>logical.
  X > Y => Y<X.
  */

  -- Not strictly necessary, but makes for better symmetry.
  public logical ::= true | false.

  all t ~~ equality[t] |: person[t] ::= someOne{ name : t. spouse: option[person[t]]. assert name \== foo}.

  foo : string.
  foo = "".
}
