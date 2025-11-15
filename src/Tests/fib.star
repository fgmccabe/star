test.fib{
  import star.
  import star.assert.
  import test.lib.timer.

  public fib:(integer)=>integer.
  fib(0) => 0.
  fib(1) => 1.
  fib(N) => _int_plus(fib(_int_minus(N,1)),fib(_int_minus(N,2))).

  main:(integer){}.
  main(V){
    timer = ref timer_start((2.0**(V::float))::integer, "fib");
    F = fib(V);
    timer_finish(timer!);
    showMsg("Fib of $(V) is $(F)");

    try{
      _jit_compile("#(__pkg__)@fib",1);
    } catch {
      X do showMsg("$(X)")
    };

    timer2 = ref timer_start((2.0**(V::float))::integer, "fib");
    fib(V);
    timer_finish(timer2!)
  }

  public _main:(cons[string]){}.
  _main([]) do main(30).
  _main([Count]) do main(Count::integer).
}
  
