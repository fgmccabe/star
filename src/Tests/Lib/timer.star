test.lib.timer{
  import star.

  public timer_start : (integer, string) => (integer, integer, string).
  timer_start(count, msg) => (_ticks(), count, msg).

  public timer_finish : ((integer, integer, string)) => integer.
  timer_finish((start, count, msg)) => valof {
    try{
      stop = _ticks();
      elapsed = ((stop - start)::float)/1.0e6;
      ops_per_sec = ((count::float) / elapsed)::integer;
      showMsg("$(count)\t#(msg)\t$(elapsed) s\t$(ops_per_sec) ops/sec");
      valis stop-start
    } catch {
      .exception(M) => showMsg("exception: $(M)")
    };
    valis -1;
  }

  public timeOf:(()=>(),string,integer)=>integer.
  timeOf(Fn,Msg,Cx) => valof{
    try{
      Start = _ticks();
      Fn();
      Stop = _ticks();
      elapsed = ((Stop - Start)::float)/1.0e6;
      ops_per_sec = ((Cx::float) / elapsed)::integer;
      showMsg("#(Msg)\t$(elapsed) s\t$(ops_per_sec) ops/sec");
      valis Stop-Start
    } catch {
      .exception(M) => showMsg("exception: $(M)")
    };
    valis -1;
  }
}
