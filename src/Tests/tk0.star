test.tk0{
  import star.
  import star.assert.
  import star.mbox.

  -- Test tasks & futures

  tk0:async ()=>string throws string.
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
	      tk2:async () =>_ throws string.
	      tk2() => valof{
		showMsg("starting tk2");
		if 3>2 then
		  valis "there"
		else
		throw "not possible"
	      }
	    } in ζ tk2);
	  F2 = waitfor(T2);
	  showMsg("final result $(F2)");
	  
	  valis ()
	} catch {
	  Msg => {
	    showMsg(Msg);
	    retire .retired_
	  }
	}
      };
	
      nursery([Tsk]);

    } catch {
      E => showMsg("$(E)")
    };
    valis ()
  }
}
