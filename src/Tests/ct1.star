test.ct1{
  -- Some simple nursery tests

  import star.
  import star.script.
  import star.structured.conn.

  ping:(integer,channel[boolean,()]) => task[()].
  ping(Cnt,Chnnl) => fiber{
    logMsg("starting $(Cnt) pings");
    Count := Cnt;

    try{
      while Count!>=0 do{
	logMsg("posting $(Count!)");
	post(this,Count!>0,Chnnl);
	Count := Count!-1
      }
    } catch { _ => logMsg("something went wrong") };
    this retire .retired_
  }

  pong:(channel[boolean,()]) => task[()].
  pong(Chnnl) => fiber{
    Count := 0;

    try{
      while collect(this,Chnnl) do{
	logMsg("received ping");
	Count := Count!+1
      }
    } catch { _ => logMsg("something went wrong") };

    logMsg("received $(Count!) pings");
    valis .result(())
  }
  
  main:()=>().
  main() => valof{
    Chnnl = newChannel();
    T1 = ping(10,Chnnl);
    T2 = pong(Chnnl);

    Rs = nursery([T1,T2]);
    logMsg("final result $(Rs)");
    valis ()
  }
}
