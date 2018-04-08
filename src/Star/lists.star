star.lists{
  import star.core.
  import star.sequence.
  import star.arith.

  implementation all x ~~ equality[x] |: equality[list[x]] => {.
    L1 == L2 where _list_size(L1)==_list_size(L2) =>
      sameList(L1,L2,0,_list_size(L1)).
    _ == _ => false.

    hash(L) => listHash(L,0,0,_list_size(L)).
  .}

  sameList:all e ~~ equality[e] |: (list[e],list[e],integer,integer)=>logical.
  sameList(_,_,X,X) => true.
  sameList(L1,L2,Ix,X) where
    _list_nth(L1,Ix) == _list_nth(L2,Ix) => sameList(L1,L2,_int_plus(Ix,1),X).
  sameList(_,_,_,_) => false.

  listHash:all e ~~ equality[e] |: (list[e],integer,integer,integer)=>integer.
  listHash(_,Hx,M,M)=>Hx.
  listHash(L,Hs,Ix,Mx) =>
    listHash(L,_int_plus(_int_times(Hs,37),
      hash(_list_nth(L,Ix))),Ix+1,Mx).

  -- stream contract
  public implementation all x ~~ stream[list[x] ->> x] => {
    _eof([]) => some(()).
    _eof(_) => none.
    _hdtl([E,..L]) => some((E,L)).
    _hdtl(_) => none.
  }

  -- display contract for lists
  public implementation all x ~~ display[x] |: display[list[x]] => {
    disp(L) => ssSeq([ss("["),..listDisp(L,"")]).
  }

  private
  listDisp:all x ~~ display[x] |: (list[x],string) => list[ss].
  listDisp([],_) => [ss("]")].
  listDisp([E,..L],Sep) => [ss(Sep),disp(E),..listDisp(L,",")].
}
