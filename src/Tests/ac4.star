test.ac4{
  import star.core.
  import star.arith.

  -- test action functions

  f:(integer) => integer throws string.
  f(X) => valof{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX .= f(X-1);
      valis XX*X
    } else
    throw "illegal arg"
  }

  main:()=>().
  main() => valof{
    try{
      F10 .= f(10);
      _ .= _logmsg(disp(F10));
      _ .= _logmsg(disp(f(-10)));
      F .= f(-10);
      _ .= _logmsg(disp(F))
    } catch {
      E => {
	_ .= _logmsg(E)
      }
    };
    valis ()
  }
  
}
