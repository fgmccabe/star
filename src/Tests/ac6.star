test.ac6{
  import star.core.
  import star.arith.
  import star.action.
  import star.iterable.

  -- test performing actions

  f:(integer) => result[string,integer].
  f(X) => do{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX <- f(X-1);
      valis XX*X
    } else
    raise "illegal arg to factorial"
  }

  dd:(integer) => result[string,()].
  dd(X) => do{
    _ .= _logmsg(disp(f(X)));
    valis ()
  }

  main:()=>().
  main() => valof{
    try{
      F10 <- f(10);
      _ .= _logmsg(disp(F10));

      perform dd(10);
    } catch {
      E => {
	_ .= _logmsg(E)
      }
    };
    valis ()
  }
  
}
