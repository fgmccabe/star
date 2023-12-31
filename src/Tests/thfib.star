test.thfib{
  import star.
  import star.assert.
  import star.vector.
  
  import test.lib.fib.

  memoFib(N) => let{.
    fibs = ({ $$(fb(ix-1)+fb(ix-2)) | ix in .range(0,N,1) }:vect[thunk[integer]]).
    fb(0) => 1.
    fb(1) => 1.
    fb(ix) where F?=fibs[ix] => F!!
  .} in fb(N-1).

  main:(integer)=>().
  main(Ix)=>valof{
    show memoFib(Ix);
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => main(10).
  _main([Count]) => main(Count::integer).
  
}
    
    

  
