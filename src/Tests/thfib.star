test.thfib{
  import star.
  import star.assert.
  import star.vector.
  
  import test.lib.fib.

  memoFib(N) => let{.
    fibs : vect[thunk[integer]].
    fibs = { $$(fb(ix-1)+fb(ix-2)) | ix in 0..<N }.

    fb(0) => 1.
    fb(1) => 1.
    fb(ix) where F?=fibs[ix] => F!!
  .} in fb(N-1).

  main:(integer){}.
  main(Ix){
    show memoFib(Ix);
  }

  public _main:(cons[string])=>integer.
  _main([]) => valof{ main(10); valis 0}.
  _main([Count]) => valof{ main(Count::integer); valis 0}.
  
}
