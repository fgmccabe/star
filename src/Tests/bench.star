test.bench{
  import star.
  import star.finger.
  import star.redblack.
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

  rbiota:(integer,integer)=>rbtree[integer,integer].
  rbiota(Mx,Mx) => [].
  rbiota(Ix,Mx) where Ix<Mx => [Ix->Ix,..rbiota(Ix+1,Mx)].

  empty:all e ~~ (e)=>action[(),()].
  empty(_) => action{
    valis ()
  }

  benchNativeList(Count) => action {
    timer .= ref timer_start(Count, "");
    idxes .= (iota(0, Count):cons[integer]);

    logMsg("******* cons lists ******");
    timer := timer_start(Count, "Creating cons list");
    cn_list .= ref (iota(0,Count):cons[integer]);
    timer_finish(timer!);

    timer := timer_start(Count, "Iterating over all elements in cons list");
    for i in (cn_list!) do {
--      logMsg("cons element: $(i)");
      empty(some(i))
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Accessing all elements in cons list");
    for i in idxes do {
      El .= cn_list![i]
--      logMsg("cons element: $(El)");
    };
    timer_finish(timer!);

    if Count =< 100000 then {
      timer := timer_start(Count, "Changing elements in cons list");
      for ix in idxes do {
        cn_list[ix] := ix + 4
      };
      timer_finish(timer!)
    };

    logMsg("******* finger trees ******");
    timer := timer_start(Count, "Creating finger tree");
    fn_list .= ref fingeriota(0,Count);
    timer_finish(timer!);
--    logMsg("finger tree: $(fn_list!)");

    timer := timer_start(Count, "Iterating over all elements in finger list");
    for i in (fn_list!) do {
--      logMsg(" element: $(i) .= $(El)")
      empty(some(i))
    };
    timer_finish(timer!);

    logMsg("measure $([|fn_list!|])");

/*
    if Count =< 100000 then {
      timer := timer_start(Count, "Changing elements in finger list");
      for ix in idxes do {
        fn_list[ix] := ix + 4
      };
      timer_finish(timer!)
    };
    */

  
    logMsg("******* skew trees ******");
    timer := timer_start(Count, "Creating skew tree");
    sk_list .= ref (iota(0,Count):sk[integer]);
    timer_finish(timer!);
--    logMsg("finger tree: $(fn_list!)");

    timer := timer_start(Count, "Iterating over all elements in skew list");
    for i in (sk_list!) do {
      empty(some(i))
--      logMsg("skew element: $(i)")
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Accessing all elements in skew list");
    for i in idxes do {
      El .= (sk_list!)[i]
--      logMsg("cons element: $(i)");
    };
    timer_finish(timer!);

    if Count =< 100000 then {
      timer := timer_start(Count, "Changing elements in skew list");
      for ix in idxes do {
        sk_list[ix] := ix + 4
      };
      timer_finish(timer!)
    };

    logMsg("******* red/black trees ******");
    timer := timer_start(Count, "Creating red/black tree");
    rb_list .= ref rbiota(0,Count);
    timer_finish(timer!);
--    logMsg("red/black tree: $(rb_list!)");

/*    timer := timer_start(Count, "Iterating over all elements in red/black list");
    for i->_ in rb_list! do {
      empty(some(i))
--      logMsg("rb element: $(i)")
    };
    timer_finish(timer!);
*/
    
    timer := timer_start(Count, "Accessing all elements in red/black list");
    for i in idxes do {
      El .= (rb_list!)[i]
--      logMsg(" element: $(i) .= $(El)")
    };
    timer_finish(timer!);

    if Count =< 100000 then {
      timer := timer_start(Count, "Changing elements in rb list");
      for ix in idxes do {
        rb_list[ix] := ix + 4
      };
      timer_finish(timer!)
    };
    valis ()
  }

  main : (integer,string) => action[(),()].
  main(Count,Msg) => action {
    logMsg("Do #(Msg) for $(Count) times");
    XX .= valof benchNativeList(Count);
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => valof main(10,"test").
  _main([Count]) => valof main(Count::integer,"test").
}





