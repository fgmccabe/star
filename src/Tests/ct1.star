test.ct1{
  -- Some simple nursery tests

  import star.
  import star.assert.
  import star.mbox.

  ping:(integer,receiver[boolean]) => (task[()])=>().
  ping(Cnt,Chnnl) => (this) => valof{
    showMsg("starting $(Cnt) pings");
    Count := Cnt;

    try{
      while Count!>=0 do{
	showMsg("posting $(Count!)");
	post(Count!>0,Chnnl);
	Count := Count!-1
      }
    } catch { _ => showMsg("something went wrong") };
    retire .retired_
  }

  pong:(emitter[boolean]) => (task[()])=>().
  pong(Chnnl) => (this) => valof{
    Count := 0;

    try{
      while collect(Chnnl) do{
	showMsg("received ping");
	Count := Count!+1
      }
    } catch { _ => showMsg("something went wrong") };

    showMsg("received $(Count!) pings");
    valis ()
  }
  
  main:()=>().
  main() => valof{
    (E,R) = newSlot();
    T1 = ping(10,R);
    T2 = pong(E);

    try{
      Rs = nursery([T1,T2]);
      showMsg("final result $(Rs)");
    } catch {
      Ex => showMsg(disp(Ex))
    };
    valis ()
  }
}
