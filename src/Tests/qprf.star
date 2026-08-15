test.qprf{
  import star.
  import star.assert.

  import test.lib.timer.

  pairs:(integer,integer) => cons[(integer,integer)].
  pairs(Mx,Stp) => { (I,I*Stp) | I in 0..<Mx }.

  gp : (cons[(integer,integer)]) => cons[(integer,integer)].
  gp(Prs) => collect{
    for (X,Z) in Prs do{
      for (Z,Y) in Prs do{
	elemis (X,Y)
      }
    }
  }

  gpq: (cons[(integer,integer)]) => cons[(integer,integer)].
  gpq(Prs) => { (X,Y) | (X,Z) in Prs && (Z,Y) in Prs }
  
  main:(integer){}.
  main(Mx){
    idxes = pairs(Mx,5);
    timer = ref timer_start(Mx, "iterating gps");
    gps = gp(idxes);
    itTime = timer_finish(timer!);
    show [|gps|];

    timer := timer_start(Mx, "query gps");
    gpsq = gpq(idxes);
    qTime = timer_finish(timer!);
    show [|gpsq|];

    try{
      showMsg("Query/Iterative = $(qTime/itTime)")
    } catch {
      .exception(M) do {
        showMsg("We got exception: $(M)")
      }
    }
  }

  public _main:(cons[string])=> integer.
  _main([]) => valof{
    main(10);
    valis 0
  }
  _main([Count]) =>
    valof{
    main(
    (try Count::integer
      catch { _ =>
	  valof{
	  _show("Cannot parse [#(Count)] as an integer");
	  valis 1
	  }
      }
      ));
    valis 0
    }
}



    

  
