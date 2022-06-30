test.lt3a{
  import star.
  import star.script.

  foo() => let{.
    X = ref genSym("x").

    test()=>X!
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
