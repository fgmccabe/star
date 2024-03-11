test.tk0{
  import star.
  import star.assert.
  import star.mbox.

  -- Test tasks & futures

  tk0:(this:task[()]),raises () |: ()=>string.
  tk0() => valof{
    logMsg("starting tk0");
    valis "hello"
  }

  main:()=>().
  main() => valof{
    try{
      Tsk = (this) => valof{
	try{
	  T = tsk(this,λtk0);
	  Fv = waitfor(T);
	  logMsg("final result $(Fv)");
	  this retire .result(())
	} catch () in {
	  _ => this retire .retired_
	}
      };
	
      Rs = nursery([Tsk]);

    } catch mboxException in {
      E => logMsg(disp(E))
    };
    valis ()
  }
}
  

  
