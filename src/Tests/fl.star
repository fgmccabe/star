test.fl{
  import star.
  import star.script.

  count:(list[list[integer]])=>integer.
--  count(L) => foldRight(acc,0,L).
  count(L) => foldRight((Ls,Cx)=>foldRight((+),Cx,Ls),0,L).

  acc:(list[integer],integer) => integer.
  acc(Ls,Cx) => foldRight((+),Cx,Ls).

  main:()=>action[(),()].
  main()=>do{
    show count([[1],[2,3],[4,5]]);

    assert count([[1],[2,3],[4,5]]) == 15
  }
}
