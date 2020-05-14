test.g{
  import star.
  import star.script.

  all c ~~ parseState[c] ::= parseState(cons[c],option[()=>parseState[c]]).

  term:all c ~~ (cons[c]) => option[(c,cons[c])].
  term([])=>.none.
  term([C,..L]) => some((C,L)).

  isTerm:all c ~~ equality[c] |:(cons[c],c) => option[cons[c]].
  isTerm([C,..L],C) => some(L).
  isTerm(_,_) => .none.

  txt:cons[integer].
  txt = "fred"::cons[integer].

  main:()=>action[(),()].
  main()=>do{
    show txt;
    assert some(_).=isTerm(txt,0cf);
    assert .none.=isTerm(txt,0cr)
  }
}
