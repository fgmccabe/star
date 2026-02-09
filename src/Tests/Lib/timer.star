test.lib.timer{
  import star.

  public timer_start : (integer, string) => (integer, integer, string).
  timer_start(count, msg) => (_ticks(), count, msg).

  public timer_finish : ((integer, integer, string)) => float.
  timer_finish((start, count, msg)) => valof {
    try{
      stop = _ticks();
      elapsed = ((stop - start)::float)/1.0e6;
      ops_per_sec = trunc((count::float) / elapsed)::integer;
      showMsg("$(count)\t#(msg)\t$(elapsed) s\t$(ops_per_sec) ops/sec");
      valis elapsed
    } catch {
      .exception(M) do showMsg("exception: $(M)")
    };
    valis -1.0;
  }

  public timeOf:((){})=>float.
  timeOf(Fn) => valof{
    try{
      Start = _ticks();
      Fn();
      Stop = _ticks();
      valis ((Stop - Start)::float)/1.0e6;
    } catch {
      .exception(M) do showMsg("exception: $(M)")
    };
    valis -1.0;
  }
}
