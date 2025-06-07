test.jt3{
  import star.
  import star.assert.
  import test.lib.timer.

  public fib:(float)=>float.
  fib(0) => 0.
  fib(1) => 1.
  fib(N) => _int_plus(fib(_int_minus(N,1)),fib(_int_minus(N,2))).

  main:(float)=>().
  main(V) => valof{
    timer = ref timer_start((Vt**2.0)::integer, "fib");
    F = fib(V);
    t1 = timer_finish(timer!)::float;
    showMsg("Fib of $(V) is $(F)");

    try{
      _jit_compile("#(__pkg__)@fib",1);
    } catch {
      X => showMsg("$(X)")
    };

    timer = ref timer_start((V**2.0::integer, "fib");
    F = fib(V);
    t2 = timer_finish(timer!)::float;
    showMsg("Fib of $(V) is $(F)");

    try {
      showMsg("Factor = $(_flt_div(t1,t2))")
    } catch { M => showMsg("Exception ") }
    
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => main(3).
  _main([Count]) => main(Count::integer).
}
    
