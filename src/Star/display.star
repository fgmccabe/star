star.display{
  import star.core.
  import star.coerce.
  import star.lists.
  import star.strings.

  public implementation coercion[ss,string] => {
    _coerce(S) => _str_multicat(unravel(S,[])).
  }

  unravel:(ss,list[string])=>list[string].
  unravel(ss(S),Rest) => [S,..Rest].
  unravel(sc(C),Rest) => [_implode([C]),..Rest].
  unravel(ssSeq(L),Rest) => unravelList(L,Rest).
  unravel(ssPr(L,R),Rest) => unravel(L,unravel(R,Rest)).

  unravelList:(list[ss],list[string]) => list[string].
  unravelList([],R) => R.
  unravelList([S,..L],R) => unravel(S,unravelList(L,R)).
}
