test.fl{
  import star.
  import star.assert.

  count:(cons[cons[integer]])=>integer.
--  count(L) => foldRight(acc,0,L).
  count(L) => foldRight((Ls,Cx)=>foldRight((+),Cx,Ls),0,L).

  acc:(cons[integer],integer) => integer.
  acc(Ls,Cx) => foldRight((+),Cx,Ls).

  main:()=>().
  main()=>valof{
    show count([[1],[2,3],[4,5]]);

    assert count([[1],[2,3],[4,5]]) == 15;
    valis ()
  }
}
