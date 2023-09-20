test.lt3{
  import star.
  import star.assert.

  foo() => let{.
    X = genSym("x").

    test()=>X
  .} in test.


  genSym(Pr) => _str_gen(Pr).

  main:()=>().
  main()=>valof{
    show foo()();
    show foo()();
    assert ~foo()()==foo()();
    valis ()
  }
}
