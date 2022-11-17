test.ac1{
  import star.core.
  import star.arith.
  import star.coerce.
  import star.tuples.

  -- Test basics of actions

  f1:(integer) => integer.
  f1(X) => valof{
    if X==1 then
      valis 1
    else
    valis f1(X-1)*X
  }

  main:()=>().
  main() => valof{
--    _logmsg(disp(f1(10)));
    X = 1;
    Y = 2;
    T = (X,Y);
    _logmsg(disp(T));
    valis ()
  }
}
    
