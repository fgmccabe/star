test.ac0{
  import star.core.
  import star.arith.
  import star.coerce.

  -- Test basics of actions

  ff:(integer) => integer.
  ff(X) => valof{
    F := 1;
    Ix := 1;
    while Ix! =< X do{
      F := F! * Ix!;
      Ix := Ix! + 1
    };
    valis F!
  }

  main:()=>().
  main() => valof{
    try{
      valis _logmsg(disp(ff(10)))
    } catch {_ => {_logmsg("error")}};
    valis ();
  }
}
    
