test.lib.timer{
  import star.

  public timer_start : (integer, string) => (integer, integer, string).
  timer_start(count, msg) => (_ticks(), count, msg).

  public timer_finish : ((integer, integer, string)) => ().
  timer_finish((start, count, msg)) => valof {
    try{
      stop = _ticks();
      elapsed = ((stop - start)::float)/1.0e6;
      ops_per_sec = ((count::float) / elapsed)::integer;
      showMsg("$(count)\t#(msg)\t$(elapsed) ms\t$(ops_per_sec) ops/sec");
    } catch exception in {
      .exception(M) => showMsg("exception: $(M)")
    };
    valis ()
  }


}
