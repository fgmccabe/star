test.ac8{
  import star.
  import star.assert.

  -- Experiments in throwing

  contract all e ~~ throwable[e] ::= {
    _throw:all x ~~ (e)=>x
  }

  f:(integer) => integer.
  f(X) => valof{
    TT = _fiber((TryTsk,())=>valof{
	let{
	  implementation throwable[integer] => {
	    _throw(E) => valof{
	      TryTsk throw E
	    }
	  }
	} in {
	  showMsg("p1");
	  if X>5 then
	    valis _throw(10)
	  else
	  valis 3*X
	}
      });
    try{
      V = TT resume ();
      valis 5*V
    } catch {
      E do {
	showMsg("err $(E)");
	valis -E
      }
    }
  }

  main:(){}.
  main(){
    showMsg("f(1) = $(f(1))");
    showMsg("f(10) = $(f(10))");
    assert f(1) == 15;
    assert f(10) == -10
  }
}

