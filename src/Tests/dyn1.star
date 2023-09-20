test.dyn1{
  import star.
  import star.assert.

  -- Experiments in throwing

  fa: _throw|=(integer)=>() |: (integer)=>integer.
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
	logMsg("we got an exception $(E)");
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
    case (TryTsk spawn valof{
	let{
	  _throw(E) => valof{
	    logMsg("retiring...");
	    TryTsk retire .err(E)
	  }
	} in {
	  logMsg("starting f($(X))");
	  TryTsk retire .ok(fe(X))
	}
      }) in {
      .err(E) => {
	logMsg("We got exception $(E)");
	valis -E
      }.
      .ok(V) =>
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
    
