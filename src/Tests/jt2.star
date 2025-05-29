test.jt2{
  import star.
  import star.assert.
  import test.lib.timer.

  public fib:(integer)=>integer.
  -- fib(N) => (
  --   0.=N ?? 0 ||
  --   1.=N ?? 1 ||
  --   _int_plus(fib(_int_minus(N,1)),fib(_int_minus(N,2)))).

  fib(0) => 0.
  fib(1) => 1.
  fib(N) => _int_plus(fib(_int_minus(N,1)),fib(_int_minus(N,2))).

  main:(integer)=>().
  main(V) => valof{
    timer = ref timer_start(((V::float)**2.0)::integer, "fib");
    F = fib(V);
    timer_finish(timer!);
    showMsg("Fib of $(V) is $(F)");

    try{
      _jit_compile("test.jt2@fib",1);
    } catch {
      X => showMsg("$(X)")
    };

    timer = ref timer_start(((V::float)**2.0)::integer, "fib");
    F = fib(V);
    timer_finish(timer!);
    showMsg("Fib of $(V) is $(F)");
    
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => main(3).
  _main([Count]) => main(Count::integer).
}
    
