test.fib{
  import star.
  import star.assert.
  import test.lib.timer.

  public fib:(integer)=>integer.
  fib(0) => 0.
  fib(1) => 1.
  fib(N) => _int_plus(fib(_int_minus(N,1)),fib(_int_minus(N,2))).

  main:(integer)=>().
  main(V) => valof{
    timer = ref timer_start((2.0**(V::float))::integer, "fib");
    F = fib(V);
    timer_finish(timer!);
    showMsg("Fib of $(V) is $(F)");

    -- try{
    --   _jit_compile(fib)
    -- } catch errorCode in {
    --   X => showMsg("$(X)")
    -- };
    -- valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => main(20).
  _main([Count]) => main(Count::integer).
}
  
