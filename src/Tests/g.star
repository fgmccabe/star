test.g{
  import star.

  all c ~~ parseState[c] ::= parseState(list[c],option[()=>parseState[c]]).

  term:all c ~~ (list[c]) => option[(c,list[c])].
  term([])=>none.
  term([C,..L]) => some((C,L)).

  isTerm:all c ~~ (list[c],c) => option[list[c]].
  isTerm([C,..L],C) => some(L).
  isTerm(_,_) => none.

  txt:list[integer].
  txt = _explode("fred").

  assert some(_) .= isTerm(txt,0cf).
}
