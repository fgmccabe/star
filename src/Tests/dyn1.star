test.dyn1{
  import star.
  import star.assert.

  -- Experiments in throwing

  fa: _throw|=(all e ~~ (integer)=>e) |: (integer)=>integer.
  fa(X) => valof{
    if X>5 then
      _throw(10)
    else
    valis 3*X
  }

  fe: _throw|=(integer)=>_ |: (integer)=>integer.
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

  all e,v ~~ rslt[e,v] ::= .ok(v) | .err(e).
  
  implementation all e,v ~~ display[e],display[v] |: display[rslt[e,v]] => {
    disp(.ok(V)) => "ok $(V)".
    disp(.err(E)) => "bad $(E)"
  }

  f:(integer) => integer.
  f(X) => valof{
    case _fiber((TryTsk,_) =>valof{
	  let{
	    _throw(E) => valof{
	      showMsg("retiring...");
	      TryTsk retire .err(E)
	    }
	  } in {
	    showMsg("starting f($(X))");
	    TryTsk retire .ok(fe(X))
	  }
	}) resume () in {
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
    
