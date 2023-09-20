test.inc{
  import star.
  import star.assert.

  double : all s ~~ (((s)=>s))=>((s)=>s).
  double(F) => (X) => F(F(X)).

  inc : (integer)=>integer.
  inc(X) => X+1.

  main:()=>().
  main()=>valof{
    show double(inc)(3);

    assert double(inc)(3)==5;
    valis ()
  }
}
