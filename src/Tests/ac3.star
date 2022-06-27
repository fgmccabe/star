test.ac3{
  import star.core.
  import star.arith.

  -- Test basics of actions

  f:(integer) => integer.
  f(X) => valof{
    try{
      _logmsg("p1");
      if X>5 then
	raise 10
      else
      valis 3*X
    } catch {
      (I) => {
	_logmsg(disp(I))
      }
    };
    valis 5*X
  } * 2.

  main:()=>().
  main() => valof{
    _ .= _logmsg(disp(f(1)));
    _ .= _logmsg(disp(f(10)));
    valis ()
  }
}
    
