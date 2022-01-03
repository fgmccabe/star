test.inc{
  import star.
  import star.script.

  double : all s ~~ (((s)=>s))=>((s)=>s).
  double(F) => (X) => F(F(X)).

  inc : (integer)=>integer.
  inc(X) => X+1.

  main:()=>action[(),()].
  main()=>action{
    show double(inc)(3);

    assert double(inc)(3)==5
  }
}
