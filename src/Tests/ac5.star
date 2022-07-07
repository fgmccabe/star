test.ac5{
  import star.core.
  import star.arith.
  import star.iterable.
  import star.range.
  import star.task.

  -- test for loops

  f:(integer) => integer.
  f(X) => valof{
    F .= ref 1;
    for Ix in range(1,X,1) do{
      F := F!*Ix
    };
    valis F!
  }

  main:()=>().
  main() => valof{
    _logmsg(disp(f(10)));
    try{
      F10 .= f(10);
      _logmsg(disp(F10));
      F .= f(-10);
      _logmsg(disp(F))
    } catch {
      E => {
	_logmsg(E)
      }
    };
    valis ()
  }
}
