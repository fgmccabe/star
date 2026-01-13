test.c1{
  import star.
  import star.script.

  fooBar() => let{.
    A = .nil.
    B = .cons(A,.nil).
  .} in B.

  main:()=>().
  main()=>valof{
    assert .nil ?= head(fooBar());
  }
}
