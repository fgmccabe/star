test.tk1{
  import star.
  import star.assert.
  import star.mbox.

  -- Test tasks & futures
  main:()=>().
  main() => valof{
    try{
      Tsk = (this) => valof{
	try{
	  T = task{
	      showMsg("starting tk0");
	      valis "hello"
	    };
	  Fv = waitfor(T);
	  showMsg("result $(Fv)");

	  T2 = task{
	    showMsg("starting tk2");
	    if 3>2 then
	      valis "there"
	    else
	    throw ()
	  };
	  F2 = waitfor(T2);
	  showMsg("final result $(F2)");
	  
	  valis ()
	} catch {
	  _ => retire .retired_
	}
      };
	
      nursery([Tsk]);

    } catch {
      E => showMsg("$(E)")
    };
    valis ()
  }
}
