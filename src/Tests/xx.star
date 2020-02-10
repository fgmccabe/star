test.xx{
  import star.

  contract all a ~~ alpha[a] ::= {
    gimme:(a)=>boolean.
  }

  implementation alpha[(boolean,integer)] => {.
    gimme((X,_)) => X.
  .}

  face ::= exists a ~~ alpha[a] |: face{get: a. gg:(a) => boolean. type a : a. }

  ineedOne(0) => face{ get = (true,0) }.
  ineedOne(1) => face{
    get = some(false).

    a ~> option[boolean].
  
    implementation alpha[option[boolean]] => {.
      gimme(some(X)) => X.
      gimme(none) => false.
    .}

    gg(x) => gimme(x).
 }.

--  show gimme(ineedOne(0).get).

--  assert \+ gimme(ineedOne(1).get).

}
