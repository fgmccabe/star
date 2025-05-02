test.ac3{
  import star.core.
  import star.arith.

  -- Test basics of actions

  f:(integer) => integer.
  f(X) => valof{
    try{
      logM("p1");
      if X>5 then
	throw 10
      else
      valis 3*X
    } catch {
      (I) => {
	logM(disp(I))
      }
    };
    valis 5*X
  } * 2.

  main:()=>().
  main() => valof{
    logM(disp(f(1)));
    logM(disp(f(10)));
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
    
