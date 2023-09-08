test.ac8a{
  import star.
  import star.script.

  import test.throwing.
  
  -- Experiments in throwing


  ff:throwable[integer] |: (integer)=>integer.
  ff(X) => valof{
    if X>5 then
      _throw(10)
    else
    valis 3*X
  }

  /*
  f(X) => valof{
    try{
      valis ff(X)
    } catch {
      E => {
	logMsg("we got an exception $(E)");
	valis -E
      }
    }
  }
  */
  
  f:(integer) => integer.
  f(X) => valof{
    case (TryTsk spawn valof{
	let{
	  implementation throwable[integer] => {
	    _throw(E) => valof{
	      logMsg("retiring...");
	      TryTsk retire .err(E)
	    }
	  }
	} in {
	  logMsg("starting f($(X))");
	  TryTsk retire .ok(ff(X))
	}
      }) in {
      .err(E) => {
	logMsg("We got exception $(E)");
	valis -E
      }
      | .ok(V) =>
	  valis 5*V
      }
  }

  main:()=>().
  main() => valof{
    logMsg(disp(f(1)));
    logMsg(disp(f(10)));
    valis ()
  }
}
    
