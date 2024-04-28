test.ac8{
  import star.
  import star.assert.
  
  -- Experiments in throwing

  all e,v ~~ rslt[e,v] ::= .ok(v) | .err(e).

  implementation all e,v ~~ display[e],display[v] |: display[rslt[e,v]] => {
    disp(.ok(V)) => "ok $(V)".
    disp(.err(E)) => "bad $(E)"
  }

  contract all e ~~ throwable[e] ::= {
    _throw:(e)=>()
  }

  f:(integer) => integer.
  f(X) => valof{
    TT = _fiber((TryTsk,())=>valof{
	let{
	  implementation throwable[integer] => {
	    _throw(E) => valof{
	      _retire(TryTsk,.err(E))
	    }
	  }
	} in {
	  showMsg("p1");
	  if X>5 then
	    _throw(10)
	  else
	  _retire(TryTsk,.ok(3*X))
	}
	});
    case _resume(TT,()) in {
      | .err(E) => {
	showMsg("err $(E)");
	valis -E
      }
      | .ok(V) =>
	valis 5*V
    }
  }

  main:()=>().
  main() => valof{
    showMsg("f(1) = $(f(1))");
    showMsg("f(10) = $(f(10))");
    valis ()
  }
}
    
