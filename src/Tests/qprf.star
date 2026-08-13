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
  
  main:(integer){}.
  main(Mx){
    timer = ref timer_start(Mx, "creating pairs");
    idxes = pairs(Mx,5);
    timer_finish(timer!);

    timer := timer_start(Mx, "creating gps");
    
    gps = gp(idxes);
    timer_finish(timer!);
    show [|gps|]
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



    

  
