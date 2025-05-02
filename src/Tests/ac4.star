test.ac4{
  import star.core.
  import star.arith.

  -- test action functions

  f:(integer) => integer throws string.
  f(X) => valof{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX = f(X-1);
      valis XX*X
    } else
    throw "illegal arg of f"
  }

  main:()=>().
  main() => valof{
    try{
      F10 = f(10);
      logM(disp(F10));
      logM(disp(f(-10)));
      F = f(-10);
      logM(disp(F))
    } catch {
      E => {
	logM(E)
      }
    };
    valis ()
  }

  logM:(string)=>().
  logM(M) => valof{
    try{
      _logmsg(M)
    } catch {_ => {}};
    valis ()
  }
}
