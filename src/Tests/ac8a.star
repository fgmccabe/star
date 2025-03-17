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
	showMsg("we got an exception $(E)");
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
	      showMsg("retiring...");
	      TryTsk retire .err(E)
	    }
	  }
	} in {
	  showMsg("starting f($(X))");
	  TryTsk retire .ok(ff(X))
	}
      });

    case Tsk resume () in {
      | .err(E) => {
	showMsg("We got exception $(E)");
	valis -E
      }
      | .ok(V) =>
	  valis 5*V
      }
  }

  main:()=>().
  main() => valof{
    show f(1);
    show f(10);
    valis ()
  }
}
    
