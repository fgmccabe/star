test.lst{
  import star.core.
  import test.ar.

  public implementation all x ~~ stream[list[x] ->> x] => {
    _eof(X) => _list_empty(X).
    _hdtl(X) where !_list_empty(X) => some((_list_nth(X,0),_list_back(X,1))).
    _hdtl(_) => .none.

    _back(X) where !_list_empty(X) && Last .=_list_size(X)-1 => some((_list_front(X,Last),_list_nth(X,Last))).
    _back(_) => .none.
  }

  public implementation all x ~~ sequence[list[x] ->> x] => {
    _cons(E,S) => _list_prepend(S,E).
    _apnd(S,E) => _list_append(S,E).

    _nil = _list_nil(2).
  }
}  
