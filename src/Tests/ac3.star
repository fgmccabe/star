test.ac3{
  import star.core.
  import star.arith.
  import star.coerce.

  -- Test basics of actions

  f:(integer) => integer.
  f(X) => valof{
    try{
      _ .= _logmsg("p1");
      raise 10
    } catch {
      (I) => {
	_ .= _logmsg(disp(I));
	valis 5*X
      }
    }
  }

  main:()=>().
  main() => valof{
    _ .= _logmsg(disp(f(10)));
    valis ()
  }
}
    
