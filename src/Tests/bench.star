test.bench{
  import star.
  import star.finger.
  
  timer_start : (integer, string) => (integer, integer, string).
  timer_start(count, msg) => (_ticks(), count, msg).

  timer_finish : ((integer, integer, string)) => action[(),integer].
  timer_finish((start, count, msg)) => do {
    stop = _ticks();
    elapsed = ((stop - start)::float)/1.0e6;
    ops_per_sec = ((count::float) / elapsed)::integer;
    logMsg("$(count)\t#(msg)\t$(elapsed) ms\t$(ops_per_sec) ops/sec");
    lift ops_per_sec
  }

  fingeriota:(integer,integer)=>fingerTree[integer].
  fingeriota(Mx,Mx) => [].
  fingeriota(Ix,Mx) where Ix<Mx => [Ix,..fingeriota(Ix+1,Mx)].

  benchNativeList(Count) => action {
    timer := timer_start(Count, "");
    idxes = iota(0, Count);

    logMsg("******* native lists ******");
    timer := timer_start(Count, "Creating native list of $(Count) elements");
    el_list := iota(0,Count);
    timer_finish(timer!); 
    ignore := el_list![0];

    logMsg("******* finger trees ******");
    timer := timer_start(Count, "Creating finger tree");
    fn_list := fingeriota(0,Count);
    timer_finish(timer!);
--    logMsg("finger tree: $(fn_list!)");

    timer := timer_start(Count, "Accessing all elements in native list");
    for i in (el_list!) do {
      ignore := some(i)
    };
    timer_finish(timer!);
    logMsg("(last element: $(ignore!) (should be: $(idxes[(Count-1)])))");

    timer := timer_start(Count, "Accessing all elements in finger list");
    for i in (fn_list!) do {
      ignore := some(i)
    };
    timer_finish(timer!);

    if Count =< 100000 then {
      logMsg("start changing");
      timer := timer_start(Count, "Changing elements in native list");
      for ix in idxes do {
        el_list[ix] := ix + 1
      };
      timer_finish(timer!);

      timer := timer_start(Count, "Copying native list of size $(size(el_list!))");
      tmp_list := el_list!;
      timer_finish(timer!);

      tmp_list[0] := 13;
      -- assert tmp_list[0]!=el_list[0];
      logMsg("tmp_list[0] should be != el_list[0]: $(tmp_list![0]) != $(el_list![0])");

      timer := timer_start(Count, "Changing elements in copy of native list");
      for ix in idxes do {
        tmp_list[ix] := ix + 2
      };
      timer_finish(timer!)
    };
    lift ()
  }

  main : (integer,string) => action[(),()].
  main(Count,Msg) => do {
    logMsg("Do #(Msg) for $(Count) times");
    XX <- benchNativeList(Count);
    lift ()
  }

  _main:(list[string])=>().
  _main([]) => valof main(10,"test").
  _main([Count]) => valof main(Count::integer,"test").

}
