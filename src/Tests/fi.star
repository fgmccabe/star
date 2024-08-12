test.fi{
  import star.
  import star.assert.

  fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>N*fact(N-1).

  main:()=>().
  main()=>valof{
    show "Fact of 3 is $(fact(3)), fact of 4 is $(fact(4))";

    assert "Fact of 3 is 6" == "Fact of 3 is $(fact(3))";
    valis ()
  }
}
