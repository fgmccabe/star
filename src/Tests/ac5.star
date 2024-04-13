test.ac5{
  import star.core.
  import star.arith.
  import star.iterable.
  import star.range.

  -- test for loops

  f:(integer) => integer.
  f(X) => valof{
    F = ref 1;
    for Ix in 1..<X do{
      F := F!*Ix
    };
    valis F!
  }

  main:()=>().
  main() => valof{
    logM(disp(f(10)));
    try{
      F10 = f(10);
      logM(disp(F10));
      F = f(-10);
      logM(disp(F))
    } catch string in {
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
    } catch errorCode in {_ => {}};
    valis ()
  }
  
}
