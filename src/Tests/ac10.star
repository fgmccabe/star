test.ac10{
  import star.
  import star.assert.

  -- Test labeled statements

  labeled:(integer) => integer.
  labeled(X) => valof{
    a:if X>=0 then {
      a:if X>0 then{
	break a;
	logMsg("should not be here");
      } else
        valis 0;
      valis 1
    };
    valis -1
  }

  firstMultiple:raises exception |:(integer,integer)=>string.
  firstMultiple(X,M) => valof{
    L:{
      for ix in .range(1,X,1) do{
	if ix%M==0 then
	  break L
      };
      valis "not found"
    };
    valis "found"
  }

  main:()=>().
  main()=>valof{
    show labeled(1);
    show labeled(-1);
    show labeled(0);
    assert labeled(1) == 1;
    assert labeled(-1) == -1;
    assert labeled(0) == 0;

    try{
      show firstMultiple(10,3);
      show firstMultiple(3,10);

      assert firstMultiple(10,3) == "found";
      assert firstMultiple(3,10) == "not found";
    } catch exception in {
      .exception(M) => logMsg("we got an exception: $(M)")
    };
    valis ()
  }
}  
