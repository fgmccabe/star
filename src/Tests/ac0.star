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

  main:()=>integer.
  main() => valof{
    try{
      _logmsg(disp(ff(10)))
    } catch {_ do {_logmsg("error")}};
    valis 0;
  }
}
    
