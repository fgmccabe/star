test.lt3a{
  import star.
  import star.script.

  foo() => let{
    X = ref genSym("x").

    test()=>X!
  } in test.


  genSym(Pr) => chrs_(_str_gen(_str_fltn(Pr))).

  main:()=>action[(),()].
  main()=>action{
    show foo()();
    show foo()();
    assert ~foo()()==foo()()
  }
}
