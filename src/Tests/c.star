test.c {
  import star.

  all t ~~ foo[t] ::= bar(t) | baz.

  all t ~~ /* equality[t] |: */person[t] ::=
    someOne{
      name : t.
      spouse: option[person[t]].
      spouse default = none.
      assert spouse=!=none
    }
    
  implementation all t ~~ equality[t] |: equality[person[t]] => {.
    P1 == P2 => P1.name == P2.name.
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

  fct:(integer)=>integer.
  fct(0)=>1.
  fct(N) => _int_times(N,fct(_int_minus(N,1))).

  tfct:(integer,integer)=>integer.
  tfct(0,X) => X.
  tfct(N,A) => tfct(_int_minus(N,1),_int_times(N,A)).

  assert fct(3)==tfct(3,1).
}
