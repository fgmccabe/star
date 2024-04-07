test.ac8a{
  import star.
  import star.assert.

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
    Tsk = _fiber((TryTsk,_)=> valof{
	let{
	  implementation throwable[integer] => {
	    _throw(E) => valof{
	      logMsg("retiring...");
	      _retire(TryTsk,.err(E))
	    }
	  }
	} in {
	  logMsg("starting f($(X))");
	  _retire(TryTsk,.ok(ff(X)))
	}
      });

    case _resume(Tsk,())in {
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
    
