test.g{
  import star.
  import star.assert.

  all c ~~ parseState[c] ::= .parseState(cons[c],option[()=>parseState[c]]).

  term:all c ~~ (cons[c]) => option[(c,cons[c])].
  term([])=>.none.
  term([C,..L]) => .some((C,L)).

  isTerm:all c ~~ equality[c] |:(cons[c],c) => option[cons[c]].
  isTerm([C,..L],C) => .some(L).
  isTerm(_,_) => .none.

  txt:cons[char].
  txt = "fred"::cons[char].

  main:()=>().
  main()=>valof{
    show txt;
    assert .some(_).=isTerm(txt,`f`);
    assert .none.=isTerm(txt,`r`);
    valis ()
  }
}
