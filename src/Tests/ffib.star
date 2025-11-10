test.ffib{
  import star.
  import star.assert.
  import test.lib.timer.

  -- Micro benchmark to enable counting of functions

  ffib:(integer)=>integer.
  ffib(0) => 1.
  ffib(1) => 1.
  ffib(N) => _int_plus(_int_plus(ffib(_int_minus(N,1)),ffib(_int_minus(N,2))),1).

  tfib:(integer,integer)=>integer.
  tfib(C,0) => _int_plus(C,1).
  tfib(C,1) => _int_plus(C,1).
  tfib(C,N) => tfib(tfib(_int_plus(C,1),_int_minus(N,1)),_int_minus(N,2)).

  main:(integer){}.
  main(V){
    OpCount = ffib(V);
    timer = ref timer_start(OpCount, "ffib");
    F = ffib(V);
    Tm1 = timer_finish(timer!);
    showMsg("FFib of $(V) is $(F)");

    ttimer = ref timer_start(OpCount, "tfib");
    Cnt = tfib(0,V);
    ttime = timer_finish(ttimer!);
    showMsg("TFib of $(V) is $(Cnt)")
  }
    

  public _main:(cons[string]){}.
  _main([]) do main(30).
  _main([Count]) do main(Count::integer).
}
  
