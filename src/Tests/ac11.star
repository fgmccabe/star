test.ac11{
  import star.
  import star.script.
  
  -- Experiments in dynamic scope

  contract all e ~~ dynamic[e] ::= {
    _dynamic:e
  }

  f:(integer) => integer.
  f(X) => valof{
    let{.
      implementation dynamic[integer] => {
	_dynamic = X
      }
    .} in { valis ff(X) }
  }

  -- A super complicated way of multiplying odd numbers
  ff:dynamic[integer] |: (integer)=>integer.
  ff(U) where U%2==0 => f(U-1).
  ff(U) where U>1 => valof{
    logMsg("dynamic = $(_dynamic:integer)");
    valis ff(U-1)*_dynamic
  }
  ff(_) default => _dynamic.

  main:()=>().
  main() => valof{
    _logmsg(disp(f(1)));
    _logmsg(disp(f(10)));

    assert f(10)==945;
    valis ()
  }
}
    
