test.ac6{
  import star.
  import star.redblack.
  import star.script.
  -- test large stacks

  rbiota:(integer,integer)=>rbtree[integer,integer].
  rbiota(Mx,Mx) => [].
  rbiota(Ix,Mx) where Ix<Mx => [Ix->Ix,..rbiota(Ix+1,Mx)].

  timer_start : (integer, string) => (integer, integer, string).
  timer_start(count, msg) => (_ticks(), count, msg).

  timer_finish : ((integer, integer, string)) => ().
  timer_finish((start, count, msg)) => valof {
    stop = _ticks();
    elapsed = ((stop - start)::float)/1.0e6;
    ops_per_sec = ((count::float) / elapsed)::integer;
    logMsg("$(count)\t#(msg)\t$(elapsed) ms\t$(ops_per_sec) ops/sec");
    valis ()
  }

  empty:all e ~~ (e)=>().
  empty(_) => ().

  large(Count) => valof {
    timer = ref timer_start(Count, "");
    idxes = (iota(0, Count):cons[integer]);
--    logMsg("Indices: $(idxes)");

    logMsg("******* red/black trees ******");
    timer := timer_start(Count, "Creating red/black tree");
    rb_list = ref rbiota(0,Count);
    timer_finish(timer!);
    logMsg("red/black tree: $(rb_list!)");

    timer := timer_start(Count, "Iterating over all elements in red/black list");
    for i->_ in rb_list! do {
      empty(?i);
    };
    timer_finish(timer!);
    
    timer := timer_start(Count, "Accessing all elements in red/black list");
    for i in idxes do {
      El = (rb_list!)[i];
--      logMsg("next element: $(El)");
    };

    timer_finish(timer!);

    if Count =< 100000 then {
      timer := timer_start(Count, "Changing elements in rb list");
      for ix in idxes do {
--	logMsg("update $((rb_list!)[ix])");
        rb_list[ix] := ix + 4;
--	logMsg("updated to $((rb_list!)[ix])");
      };
      timer_finish(timer!)
    };

    valis ()
  }

  main : (integer,string) => ().
  main(Count,Msg) => valof {
    logMsg("Do #(Msg) for $(Count) times");
    valis large(Count);
  }

  public _main:(cons[string])=>().
  _main([]) => main(10,"test").
  _main([Count]) => main(Count::integer,"test").
}


