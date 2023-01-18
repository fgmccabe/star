test.ct0{
  -- Some simple nursery tests

  import star.
  import star.script.
  import star.structured.conn.

  tt:(integer)=>task[()].
  tt(K) => fiber{
    Count := K;
    try{
      while Count!>0 do{
	pause();
	Count := Count! -1;
	logMsg("$(K) moving along, $(Count!) rounds left");
      }
    } catch { .canceled => {
	logMsg("$(K) shutting down");
      }
    };
    valis .result(())
    }.
  
  main:()=>().
  main() => valof{
    T1 = tt(10);
    T2 = tt(20);

    Rs = nursery([T1,T2]);
    logMsg("final result $(Rs)");
    valis ()
  }
}
