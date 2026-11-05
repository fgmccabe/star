test.dyn1{
  import star.
  import star.assert.

  -- Experiments in throwing

  fa: (_throw:(all e ~~ (integer)=>e)) |= (integer)=>integer.
  fa(X) => valof{
    if X>5 then
      _throw(10);

    valis 3*X
  }

  fe: (_throw:(integer)=>_) |= (integer)=>integer.
  fe(X) => (X>5 ?? _throw(10) || 3*X).

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
    try{
      V = _fiber((TryTsk,_) =>valof{
	  let{
	    _throw:(integer)=>integer.
	    _throw(E) => valof{
	      showMsg("retiring...");
	      TryTsk throw E
	    }
	  } in {
	    showMsg("starting f($(X))");
	    valis fe(X)
	  }
	}) resume ();
      valis 5*V
    } catch {
      E do {
	showMsg("We got exception $(E)");
	valis -E
      }
    }
  }

  main:(){}.
  main(){
    show f(1);
    show f(10);
    assert f(1) == 15;
    assert f(10) == -10
  }
}

