test.bench{
  import star.
  import star.finger.
  import star.redblack.
  import star.skew.
  import star.vector.
  import star.assert.
  
  timer_start : (integer, string) => (integer, integer, string).
  timer_start(count, msg) => (_ticks(), count, msg).

  timer_finish : ((integer, integer, string)) => ().
  timer_finish((start, count, msg)) => valof {
    try{
      stop = _ticks();
      elapsed = ((stop - start)::float)/1.0e6;
      ops_per_sec = ((count::float) / elapsed)::integer;
      logMsg("$(count)\t#(msg)\t$(elapsed) ms\t$(ops_per_sec) ops/sec");
    } catch exception in {
      .exception(M) => logMsg("exception: $(M)")
    };
    valis ()
  }

  fingeriota:(integer,integer)=>fingerTree[integer].
  fingeriota(Mx,Mx) => [].
  fingeriota(Ix,Mx) where Ix<Mx => [Ix,..fingeriota(Ix+1,Mx)].

  idealIota:(integer,integer)=>map[integer,integer].
  idealIota(Mx,Mx) => [].
  idealIota(Ix,Mx) where Ix<Mx => idealIota(Ix+1,Mx)[Ix->Ix].

  rbiota:(integer,integer)=>rbtree[integer,integer].
  rbiota(Mx,Mx) => [].
  rbiota(Ix,Mx) where Ix<Mx => [Ix->Ix,..rbiota(Ix+1,Mx)].

  empty:all e ~~ (e)=>().
  empty(_) => ().

  benchNativeList(Count) => valof {
    timer = ref timer_start(Count, "");
    idxes = (iota(0, Count):cons[integer]);

    logMsg("******* cons lists ******");
    timer := timer_start(Count, "Creating cons list");
    cn_list = ref (iota(0,Count):cons[integer]);
    timer_finish(timer!);

    timer := timer_start(Count, "Iterating over all elements in cons list");
    for i in (cn_list!) do {
--      logMsg("cons element: $(i)");
      empty(.some(i))
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Accessing all elements in cons list");
    for i in idxes do {
      El = cn_list![i]
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

    logMsg("******* ideal hash ******");
    timer := timer_start(Count, "Creating ideal tree");
    id_list = ref idealIota(0,Count);
    timer_finish(timer!);
--    logMsg("ideal map: $(id_list!)");

    timer := timer_start(Count, "Iterating over all elements in ideal");
    for (i->_) in (id_list!) do {
--      logMsg(" element: $(i) = $(El)")
      empty(.some(i))
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Accessing all elements in ideal");
    for i in idxes do {
      El = id_list![i];
--      logMsg(" element: $(i) = $(El)")
      empty(.some(i))
    };
    timer_finish(timer!);

--    logMsg("measure $([|id_list!|])");

    if Count =< 100000 then {
      timer := timer_start(Count, "Changing elements in ideal map");
      for ix in idxes do {
        id_list[ix] := ix + 4
      };
      timer_finish(timer!)
    };

    logMsg("******* finger trees ******");
    timer := timer_start(Count, "Creating finger tree");
    fn_list = ref fingeriota(0,Count);
    timer_finish(timer!);
--    logMsg("finger tree: $(fn_list!)");

    timer := timer_start(Count, "Iterating over all elements in finger list");
    for i in (fn_list!) do {
--      logMsg(" element: $(i) = $(El)")
      empty(.some(i))
    };
    timer_finish(timer!);

--     timer := timer_start(Count, "Accessing all elements in finger list");
--     for i in idxes do {
--       El = fn_list![i]
--     };
--     timer_finish(timer!);

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
    sk_list = ref (iota(0,Count):sk[integer]);
    timer_finish(timer!);
--    logMsg("finger tree: $(fn_list!)");

    timer := timer_start(Count, "Iterating over all elements in skew list");
    for i in (sk_list!) do {
      empty(.some(i))
--      logMsg("skew element: $(i)")
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Accessing all elements in skew list");
    for i in idxes do {
      El = (sk_list!)[i]
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
    rb_list = ref rbiota(0,Count);
    timer_finish(timer!);
--    logMsg("red/black tree: $(rb_list!)");

    timer := timer_start(Count, "Iterating over all elements in red/black list");
    for i->_ in rb_list! do {
      empty(.some(i))
--      logMsg("rb element: $(i)")
    };
    timer_finish(timer!);
    
    timer := timer_start(Count, "Accessing all elements in red/black list");
    for i in idxes do {
      El = (rb_list!)[i]
--      logMsg(" element: $(i) = $(El)")
    };
    timer_finish(timer!);

    if Count =< 100000 then {
      timer := timer_start(Count, "Changing elements in rb list");
      for ix in idxes do {
        rb_list[ix] := ix + 4
      };
      timer_finish(timer!)
    };

    logMsg("******* vectors ******");
    timer := timer_start(Count, "Creating vector");
    v = ref idxes::vect[integer];

    timer_finish(timer!);
--    logMsg("vector: $(v!)");

    timer := timer_start(Count, "Iterating over all elements in vector");
    for i in v! do {
      empty(.some(i));
--      logMsg("vector element: $(i)")
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Accessing all elements in vector");
    for i in idxes do {
      El = v![i]
--      logMsg(" element: $(i) = $(El)")
    };
    timer_finish(timer!);

    timer := timer_start(Count, "Changing elements in vector");
    for ix in idxes do {
      v[ix] := ix+4;
--      v := vupdate(v!,ix,ix+4);
    };
    timer_finish(timer!);
    
    valis ()
  }

  main : (integer,string) => ().
  main(Count,Msg) => valof {
    logMsg("Do #(Msg) for $(Count) times");
    benchNativeList(Count);
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => main(10,"test").
  _main([Count]) => main(Count::integer,"test").
}





