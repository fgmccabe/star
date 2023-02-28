test.ac8{
  import star.
  import star.script.
  
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
    TT = _new_fiber((TryTsk,_) => valof{
	let{
	  implementation throwable[integer] => {
	    _throw(E) => valof{
	      _retire(TryTsk,.err(E))
	    }
	  }
	} in {
	  _logmsg("p1");
	  if X>5 then
	    _throw(10)
	  else
	  valis .ok(3*X)
	}
	});
    case _resume(TT,()) in {
      .err(E) => {
	_logmsg(disp(E));
	valis -E
      }.
      .ok(V) =>
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
    
