test.ac4{
  import star.core.
  import star.arith.

  -- test action functions

  f:raises string |: (integer) => integer.
  f(X) => valof{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX = f(X-1);
      valis XX*X
    } else
    raise "illegal arg of f"
  }

  main:()=>().
  main() => valof{
    try{
      F10 = f(10);
      _logmsg(disp(F10));
      _logmsg(disp(f(-10)));
      F = f(-10);
      _logmsg(disp(F))
    } catch string in {
      E => {
	_logmsg(E)
      }
    };
    valis ()
  }
  
}
