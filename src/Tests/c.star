test.c {
  import star.core.

  public implementation all t ~~ equality[t] |: equality[option[t]] => {.
    none == none => true.
    some(X) == some(Y) => X==Y.
    _ == _ => false.

    hash(X) => optHash(X).
  .}

  optHash:all t ~~ equality[t] |: (option[t]) => integer.
  optHash(none) => 0.
  optHash(some(X)) => hash(X).

  public all t ~~ option[t] ::= none | some(t).

  public contract all x ~~ equality[x] ::= {
    (==): (x,x)=>boolean.
    hash: (x)=>integer.
  }

  public (=!=):all x ~~ equality[x] |: (x,x)=>boolean.
  x =!= y => \+ x==y.

  public implementation equality[string] => {
    X == Y => _str_eq(X,Y).
    hash(X) => _str_hash(X).
  }

  all t ~~ equality[t] |: person[t] ::=
    someOne{
      name : t.
      spouse: option[person[t]].
      spouse default = none.
      assert spouse=!=none
    }

  implementation all t ~~ equality[t] |: equality[person[t]] => {.
    P1 == P2 => P1.name == P2.name.
    hash(P) => hash(P.name).
  .}

  foo : string.
  foo = "".

  fp : person[string].
  fp = someOne{ name = foo. spouse = none. /*assert name == foo*/}

  fper:(string)=>person[string].
  fper(W) => someOne{name = W. spouse=none}.

  assert fp.name == "".
  assert fp.spouse == none.

  assert fper("fred").name == "fred".

  id: all x ~~ (x)=>x.
  id(X) => X.

  public implementation equality[integer] => {
    X == Y => _int_eq(X,Y).
    hash(X) => X.
  }

  fct:(integer)=>integer.
  fct(0)=>1.
  fct(N) => _int_times(N,fct(_int_minus(N,1))).

  tfct:(integer,integer)=>integer.
  tfct(0,X) => X.
  tfct(N,A) => tfct(_int_minus(N,1),_int_times(N,A)).

  assert fct(3)==tfct(3,1).
}
