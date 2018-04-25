star.lists{
  import star.core.
  import star.arith.

  public implementation all x ~~ equality[x] |: equality[list[x]] => {.
    L1 == L2 where _list_size(L1)==_list_size(L2) =>
      sameList(L1,L2,0,_list_size(L1)).
    _ == _ => false.
  .}

  public implementation all x ~~ hash[x] |: hash[list[x]] => {
    hash(L) => listHash(L,0,0,_list_size(L)).
  }

  sameList:all e ~~ equality[e] |: (list[e],list[e],integer,integer)=>boolean.
  sameList(_,_,X,X) => true.
  sameList(L1,L2,Ix,X) where
    _list_nth(L1,Ix) == _list_nth(L2,Ix) => sameList(L1,L2,_int_plus(Ix,1),X).
  sameList(_,_,_,_) => false.

  listHash:all e ~~ hash[e] |: (list[e],integer,integer,integer)=>integer.
  listHash(_,Hx,M,M)=>Hx.
  listHash(L,Hs,Ix,Mx) =>
    listHash(L,_int_plus(_int_times(Hs,37),
      hash(_list_nth(L,Ix))),Ix+1,Mx).

  -- stream contract
  public implementation all x ~~ stream[list[x] ->> x] => {
    _eof() <= (X) where _list_empty(X).
    _hdtl(_list_nth(X,0),_list_back(X,1)) <= (X) where \+_list_empty(X).

    _cons(E,S) => _list_prepend(S,E).
    _apnd(S,E) => _list_append(S,E).

    _back(_list_front(X,Last),_list_nth(X,Last)) <= (X) where \+_list_empty(X) && Last .= _list_size(X)-1.
    _nil = _list_nil(2).
  }

  -- display contract for lists
  public implementation all x ~~ display[x] |: display[list[x]] => {
    disp(L) => ssSeq([ss("["),..listDisp(L,"")]).
  }

  private
  listDisp:all x ~~ display[x] |: (list[x],string) => list[ss].
  listDisp([],_) => [ss("]")].
  listDisp([E,..L],Sep) => [ss(Sep),disp(E),..listDisp(L,",")].

  public implementation all x ~~ concat[list[x]] => {
    L1 ++ L2 => _list_concat(L1,L2).
  }
}
