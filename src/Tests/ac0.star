test.ac0{
  import star.core.
  import star.arith.
  import star.coerce.

  -- Test basics of actions

  ff:(integer) => integer.
  ff(X) => valof{
    F .= ref 1;
    Ix .= ref 1;
    while Ix! =< X do{
      F := F! * Ix!;
      Ix := Ix! + 1
    };
    valis F!
  }

  main:()=>().
  main() => valof{
    _ .= _logmsg(disp(ff(10)));
    valis ()
  }
}
    
