test.ac4{
  import star.core.
  import star.arith.
  import star.action.

  -- test action functions

  f:(integer) => result[string,integer].
  f(X) => do{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX <- f(X-1);
      valis XX*X
    } else
    raise "illegal arg"
  }

  main:()=>().
  main() => valof{
    _ .= _logmsg(disp(f(10)));
    try{
      F10 <- f(10);
      _ .= _logmsg(disp(F10));
      _ .= _logmsg(disp(f(-10)));
      F <- f(-10);
      _ .= _logmsg(disp(F))
    } catch {
      E => {
	_ .= _logmsg(E)
      }
    };
    valis ()
  }
  
}
