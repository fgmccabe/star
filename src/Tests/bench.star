test.bench{
  timer_start : (integer, string) => (float, integer, string);
  timer_start(count, msg) => (_ticks(), count, msg);

  timer_finish : (float, integer, string) => action[(),()];
  timer_finish(start, count, msg) => do {
    stop = _ticks();
    elapsed = (stop - start) / 1.0e6;
    ops_per_sec = (count :: float) / elapsed * 1.0e3;
    logMsg(info, "$(count)t#msg\t$elapsed ms\t$(ops_per_sec) ops/sec");
  };

  # #(for ?i in iota(?start,?stop,?step) do ?A)# ==> {
    var i := start;
    while i<= stop do{
      A;
      i := i + step;
    }
  };
  
  benchNativeList(Count) => action {
    timer := timer_start(Count, "");
    unsafe var i:= nonInteger;
    idxes is iota(0, Count-1, 1);

    logMsg(info, "******* native lists ******");
    timer := timer_start(Count, "Creating native list from iota($Count)");
    var el_list := iota(0,Count-1, 1);
    timer_finish(timer);

    var ignore := el_list[0];

    timer := timer_start(Count, "Accessing all elements in native list");
    for i in el_list do {
      ignore := i;
    }
    timer_finish(timer);
    logMsg(info, "(last element: #ignore (should be: #(idxes[(Count-1)])))");

    if Count <= 100000 then {
      timer := timer_start(Count, "Changing elements in native list");
      for ix in iota(0, Count-1, 1) do {
        el_list[ix] := ix + 1;
      }
      timer_finish(timer);

      timer := timer_start(Count, "Copying native list of size #(size(el_list))");
      var tmp_list := el_list;
      timer_finish(timer);

      tmp_list[0] := 13;
      -- assert tmp_list[0]!=el_list[0];
      logMsg(info, "tmp_list[0] should be != el_list[0]: #(tmp_list[0]) != #(el_list[0])");

      timer := timer_start(Count, "Changing elements in copy of native list");
      for ix in iota(0, Count-1, 1) do {
        tmp_list[ix] := ix + 2;
      }
      timer_finish(timer);
    }

  }

  main : action(integer,string);
  main (Count,Msg) {
    logMsg(info,"Do #Msg for $Count times");
    benchNativeList(Count);
  }

}
