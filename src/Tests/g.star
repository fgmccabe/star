test.g{
  import star.
  import star.script.

  all c ~~ parseState[c] ::= parseState(list[c],option[()=>parseState[c]]).

  term:all c ~~ (list[c]) => option[(c,list[c])].
  term([])=>.none.
  term([C,..L]) => some((C,L)).

  isTerm:all c ~~ equality[c] |:(list[c],c) => option[list[c]].
  isTerm([C,..L],C) => some(L).
  isTerm(_,_) => .none.

  txt:list[integer].
  txt = "fred"::list[integer].

  main:()=>action[(),()].
  main()=>do{
    assert isTerm(txt,0cf)=.some(_)
  }
}
