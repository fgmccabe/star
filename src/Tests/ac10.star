test.ac10{
  import star.
  import star.script.

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

  main:()=>().
  main()=>valof{
    show labeled(1);
    show labeled(-1);
    show labeled(0);
    assert labeled(1) == 1;
    assert labeled(-1) == -1;
    assert labeled(0) == 0;
    valis ()
  }
}  
