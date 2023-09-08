test.ac3{
  import star.core.
  import star.arith.

  -- Test basics of actions

  f:(integer) => integer.
  f(X) => valof{
    try{
      logM("p1");
      if X>5 then
	raise 10
      else
      valis 3*X
    } catch integer in {
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
    } catch errorCode in {_ => {}};
    valis ()
  }
}
    
