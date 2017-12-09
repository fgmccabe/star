star.core {
  public implementation all t ~~ equality[t] |: equality[option[t]] => {.
    none == none => true.
    some(X) == some(Y) => X==Y.
    _ == _ => false.

    hash(X) => optHash(X).
  .}

  public all t ~~ option[t] ::= none | some(t).

  public contract all x ~~ equality[x] ::= {
    (==): (x,x)=>logical.
    hash: (x)=>integer.
  }

  public (\==):all x ~~ equality[x] |: (x,x)=>logical.
  x \== y => \+ x==y.

  public implementation equality[string] => {
    X == Y => _str_eq(X,Y).
    hash(X) => _str_hash(X).
  }

  optHash:all t ~~ equality[t] |: (option[t]) => integer.
  optHash(none) => 0.
  optHash(some(X)) => hash(X).

  -- Not strictly necessary, but makes for better symmetry.
  public logical ::= true | false.

  all t ~~ equality[t] |: person[t] ::=
    someOne{
      name : t.
      spouse: option[person[t]].
      spouse default = none.
      assert spouse\==none
    }

  foo : string.
  foo = "".

  fp : person[string].
  fp = someOne{ name = foo. spouse = none. /*assert name == foo*/}

  assert fp.name == "".

  id: all x ~~ (x)=>x.
  id(X) => X.

  public implementation equality[integer] => {
    X == Y => _int_eq(X,Y).
    hash(X) => X.
  }

  fct:(integer)=>integer.
  fct(0)=>1.
  fct(N) => _int_times(N,fct(_int_minus(N,1))).

  assert fct(3)==6.
}
