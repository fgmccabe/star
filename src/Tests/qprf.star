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

  gpf:(cons[(integer,integer)]) => cons[(integer,integer)].
  gpf(Prs) =>
    foldLeft(((X,Z),L) =>
      foldLeft(((ZZ,Y),LL)=>
	(Z==ZZ ?? .cons((X,Y),LL) || LL),
      L,Prs),.nil,Prs).

  gpq: (cons[(integer,integer)]) => cons[(integer,integer)].
  gpq(Prs) => { (X,Y) | (X,Z) in Prs && (Z,Y) in Prs }
  
  main:(integer){}.
  main(Mx){
    idxes = pairs(Mx,5);
    timer = ref timer_start(Mx*Mx, "iterating gps");
    gps = gp(idxes);
    itTime = timer_finish(timer!);

    timer := timer_start(Mx*Mx, "folding gps");
    gpfld = gpf(idxes);
    fTime = timer_finish(timer!);

    timer := timer_start(Mx*Mx, "query gps");
    gpsq = gpq(idxes);
    qTime = timer_finish(timer!);

    assert [|gps|] == [|gpfld|] && [|gpfld|] == [|gpsq|];

    try{
      showMsg("Query/Iterative = $(qTime/itTime)");
      showMsg("Query/Folding = $(qTime/fTime)");
      showMsg("Folding/Iterative = $(fTime/itTime)");
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



    

  
