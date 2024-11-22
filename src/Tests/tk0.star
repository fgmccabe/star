test.tk0{
  import star.
  import star.assert.
  import star.mbox.

  -- Test tasks & futures

  tk0:async ()=>string raises ().
  tk0() => valof{
    showMsg("starting tk0");
    valis "hello"
  }

  main:()=>().
  main() => valof{
    try{
      Tsk = (this) => valof{
	try{
	  T = tsk(this,ζ tk0);
	  Fv = waitfor(T);
	  showMsg("result $(Fv)");

	  T2 = tsk(this,let{
	      tk2:async () =>_ raises ().
	      tk2() => valof{
		showMsg("starting tk2");
		if 3>2 then
		  valis "there"
		else
		raise ()
	      }
	    } in ζ tk2);
	  F2 = waitfor(T2);
	  showMsg("final result $(F2)");
	  
	  valis ()
	} catch () in {
	  _ => _retire(this,.retired_)
	}
      };
	
      nursery([Tsk]);

    } catch mboxException in {
      E => showMsg("$(E)")
    };
    valis ()
  }
}
