test.ac11{
  import star.
  import star.assert.
  
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

  isEven(X) => (try
      X%2==0
    catch {
      _ => .false
    }).

  -- A super complicated way of multiplying odd numbers
  ff:dynamic[integer] |: (integer)=>integer.
  ff(U) where isEven(U) => f(U-1).
  ff(U) where U>1 => valof{
    showMsg("dynamic = $(_dynamic:integer)");
    valis ff(U-1)*_dynamic
  }
  ff(_) default => _dynamic.

  main:()=>().
  main() => valof{
    show f(1);
    show f(10);

    assert f(10)==945;
    valis ()
  }
}
    
