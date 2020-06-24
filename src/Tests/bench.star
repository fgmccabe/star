test.bench{
  import star.
  import star.finger.
  import star.skew.
  import star.script.
  
  timer_start : (integer, string) => (integer, integer, string).
  timer_start(count, msg) => (_ticks(), count, msg).

  timer_finish : ((integer, integer, string)) => action[(),()].
  timer_finish((start, count, msg)) => do {
    stop .= _ticks();
    elapsed .= ((stop - start)::float)/1.0e6;
    ops_per_sec .= ((count::float) / elapsed)::integer;
    logMsg("$(count)\t#(msg)\t$(elapsed) ms\t$(ops_per_sec) ops/sec")
  }

  fingeriota:(integer,integer)=>fingerTree[integer].
  fingeriota(Mx,Mx) => [].
  fingeriota(Ix,Mx) where Ix<Mx => [Ix,..fingeriota(Ix+1,Mx)].

  ignore:all e ~~ (e)=>action[(),()].
  ignore(_) => do{
    valis ()
  }

  benchNativeList(Count) => action {
    timer .= ref timer_start(Count, "");
    idxes .= (iota(0, Count):cons[integer]);

    logMsg("******* cons lists ******");
    timer := timer_start(Count, "Creating cons list");
    cn_list .= ref (iota(0,Count):cons[integer]);
    timer_finish(timer!);

    logMsg("******* finger trees ******");
    timer := timer_start(Count, "Creating finger tree");
    fn_list .= ref fingeriota(0,Count);
    timer_finish(timer!);
--    logMsg("finger tree: $(fn_list!)");

    logMsg("******* skew trees ******");
    timer := timer_start(Count, "Creating skew tree");
    sk_list .= ref (iota(0,Count):sk[integer]);
    timer_finish(timer!);
--    logMsg("finger tree: $(fn_list!)");

    timer := timer_start(Count, "Accessing all elements in cons list");
    for i in (cn_list!) do {
--      logMsg("cons element: $(i)");
      ignore(some(i))
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Accessing all elements in finger list");
    for i in (fn_list!) do {
      ignore(some(i))
--      logMsg("finger element: $(i)")
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Accessing all elements in skew list");
    for i in (sk_list!) do {
      ignore(some(i))
--      logMsg("skew element: $(i)")
    };
    timer_finish(timer!);

    if Count =< 100000 then {
      logMsg("start changing");
      timer := timer_start(Count, "Changing elements in skew list");
      for ix in idxes do {
        sk_list[ix] := ix + 4
      };
      timer_finish(timer!)
      
    };
    valis ()
  }

  main : (integer,string) => action[(),()].
  main(Count,Msg) => do {
    logMsg("Do #(Msg) for $(Count) times");
    XX <- benchNativeList(Count);
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => valof main(10,"test").
  _main([Count]) => valof main(Count::integer,"test").
}





