test.lt3{
  import star.
  import star.script.

  foo() => let{.
    X = genSym("x").

    test()=>X
  .} in test.


  genSym(Pr) => _str_gen(Pr).

  main:()=>action[(),()].
  main()=>action{
    show foo()();
    show foo()();
    assert ~foo()()==foo()()
  }
}
