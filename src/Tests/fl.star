test.fl{
  import star.

  count:(list[list[integer]])=>integer.
--  count(L) => foldRight(acc,0,L).
  count(L) => foldRight((Ls,Cx)=>foldRight((+),Cx,Ls),0,L).

  acc:(list[integer],integer) => integer.
  acc(Ls,Cx) => foldRight((+),Cx,Ls).
}
