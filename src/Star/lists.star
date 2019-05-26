star.lists{
  import star.core.
  import star.arith.
  import star.iterable.
  import star.monad.

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

  -- stream & sequence contract implementations
  public implementation all x ~~ stream[list[x] ->> x] => {
    _eof(X) => _list_empty(X).
    _hdtl(X) where \+_list_empty(X) => some((_list_nth(X,0),_list_back(X,1))).
    _hdtl(_) => none.

    _back(X) where \+_list_empty(X) && Last .=_list_size(X)-1 => some((_list_front(X,Last),_list_nth(X,Last))).
    _back(_) => none.
  }

  public implementation all x ~~ sequence[list[x] ->> x] => {
    _cons(E,S) => _list_prepend(S,E).
    _apnd(S,E) => _list_append(S,E).

    _nil = _list_nil(2).
  }

  public implementation all e ~~ sizeable[list[e]] => {
    size(L) => _list_size(L).
    isEmpty(L) => _list_empty(L).
  }

  public implementation all e ~~ iter[list[e]->>e] => {
    _iter:all x,m/1,er ~~ execution[m->>er] |: (list[e],m[x],(e,x)=>m[x]) => m[x].
    _iter(Lst,St,Fn) => iterOverList(Lst,0,size(Lst),St,Fn).

    iterOverList:all x,m/1,er ~~ execution[m->>er] |:
        (list[e],integer,integer,m[x],(e,x)=>m[x]) => m[x].
    iterOverList(_,Ix,Mx,St,_) where Ix>=Mx => St.
    iterOverList(Lst,Ix,Mx,St,Fn) where El.=_list_nth(Lst,Ix) => -- do not have collections here
      _sequence(St,(SS)=>iterOverList(Lst,Ix+1,Mx,Fn(El,SS),Fn)).
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

  public flatten: all x ~~ (list[list[x]])=>list[x].
  flatten(L) => _list_flatten(L).

  public implementation all t ~~ reversible[list[t]] => {
    reverse(L) => _list_reverse(L).
  }

  public contains:all e~~equality[e] |: (e,list[e]) => boolean.
  contains(X,[X,.._]) => true.
  contains(X,[_,..L]) => contains(X,L).
  contains(_,_) default => false.

  public implementation all x ~~ head[list[x]->>x] => {
    head(X) where \+_list_empty(X) => some(_list_nth(X,0)).
    head(_) => none.

    tail(X) where \+_list_empty(X) => some(_list_back(X,1)).
    tail(_) => none.
  }

  public subtract:all e ~~ equality[e] |: (e,list[e]) => list[e].
  subtract(El,L) where Ix ^= indexOf(El,L) => _list_remove(L,Ix).

  public indexOf:all e ~~ equality[e] |: (e,list[e]) => option[integer].
  indexOf(El,L) => let{
    ixOf(Mx,Mx) => none.
    ixOf(Ix,Mx) where Ix<Mx =>
      (_list_nth(L,Ix) == El ? some(Ix) ||
	ixOf(Ix+1,Mx)).
  } in ixOf(0,size(L)).
}
