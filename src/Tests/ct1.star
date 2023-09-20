test.ct1{
  -- Some simple nursery tests

  import star.
  import star.assert.
  import star.mbox.

  ping:(integer,channel[boolean]) => (task[()])=>().
  ping(Cnt,Chnnl) => (this) => valof{
    logMsg("starting $(Cnt) pings");
    Count := Cnt;

    try{
      while Count!>=0 do{
	logMsg("posting $(Count!)");
	post(Count!>0,Chnnl);
	Count := Count!-1
      }
    } catch mboxException in { _ => logMsg("something went wrong") };
    this retire .retired_
  }

  pong:(channel[boolean]) => (task[()])=>().
  pong(Chnnl) => (this) => valof{
    Count := 0;

    try{
      while collect(Chnnl) do{
	logMsg("received ping");
	Count := Count!+1
      }
    } catch mboxException in { _ => logMsg("something went wrong") };

    logMsg("received $(Count!) pings");
    valis ()
  }
  
  main:()=>().
  main() => valof{
    Chnnl = newChannel();
    T1 = ping(10,Chnnl);
    T2 = pong(Chnnl);

    try{
      Rs = nursery([T1,T2]);
      logMsg("final result $(Rs)");
    } catch mboxException in {
      E => logMsg(disp(E))
    };
    valis ()
  }
}
