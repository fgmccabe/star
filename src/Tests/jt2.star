test.jt2{
  import star.
  import star.assert.
  import test.lib.timer.

  public fib:(integer)=>integer.
  fib(0) => 0.
  fib(1) => 1.
  fib(N) => _int_plus(fib(_int_minus(N,1)),fib(_int_minus(N,2))).

  main:(integer){}.
  main(V){
    timer = ref timer_start(trunc((V::float)**2.0):?integer, "fib");
    F1 = fib(V);
    t1 = timer_finish(timer!):?float;
    showMsg("Fib of $(V) is $(F1)");

    try{
      _jit_compile("#(__pkg__)@fib",1);
    } catch {
      | .eNOPERM do showMsg("JIT not enabled")
      | Cde do showMsg("We got errr: $(Cde)")
    };

    timer2 = ref timer_start(trunc((V::float)**2.0):?integer, "jit fib");
    F2 = fib(V);
    t2 = timer_finish(timer2!):?float;
    showMsg("Fib of $(V) is $(F2)");
    assert F1==F2;

    try {
      showMsg("Factor = $(_flt_div(t1,t2))")
    } catch { M do showMsg("Exception ") }
  }

  public _main:(cons[string])=>integer.
  _main([]) => valof{ main(3); valis 0}
  _main([Count]) => valof{ main(Count:?integer); valis 0}
}
    
