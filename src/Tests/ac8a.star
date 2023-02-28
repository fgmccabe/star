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
    case _spawn((TryTsk) => valof{
	let{
	  implementation throwable[integer] => {
	    _throw(E) => valof{
	      logMsg("retiring...");
	      _retire(TryTsk,.err(E))
	    }
	  }
	} in {
	  logMsg("starting f($(X))");
	  valis .ok(ff(X))
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
    _logmsg(disp(f(1)));
    _logmsg(disp(f(10)));
    valis ()
  }
}
    
