star.p1{
  import star.
  import star.parse.

  p:parser[(integer,integer)].
  p = item >>= (C) =>
      item >>= (_) =>
      item >>= (D) =>
      return (C,D).

  assert parse(p,[1,2,3]) == [((1,3),[])].

  q:parser[()].
  q = chr(0c() >>= (_) => chr(0c)) >>= (_) => return ().

  (in):all e ~~ equality[e] |: (e,list[e])=>boolean.
  E in L => _is_member(E,L,0,_list_size(L)).

  _is_member:all e ~~ equality[e] |: (e,list[e],integer,integer) => boolean.
  _is_member(_,_,Ix,Ix) => false.
  _is_member(E,L,Ix,_) where _list_nth(L,Ix)==E => true.
  _is_member(E,L,Ix,Lx) => _is_member(E,L,Ix+1,Lx).

  assert parse(q,[0c(,0c)]) == [((),[])].

  assert parse(str("alpha"),"alpha0"::list[integer]) == [((),[0c0])].

  assert (([(),()]:list[()]),[]) in parse(many(str("a")),"aa"::list[integer]).

  assert parse(many(str("a")),"aab"::list[integer]) == [(([(),()]),[0cb])]
}
