test.act0{
  import star.
  import star.actor.
  import star.mbox.
  import star.assert.

  -- First test of actors

  pingPong ::= .ping | .pong.

  implementation display[pingPong] => {
    disp(.ping) => "ping".
    disp(.pong) => "pong".
  }

  implementation equality[pingPong] => {
    .ping == .ping => .true.
    .pong == .pong => .true.
    _ == _ default => .false.
  }

  -- ping actor, replies with a pong for every ping, and vice versa

  pingBody::=pingBody{
    ping:()=>pingPong.
    pong:()=>pingPong
  }

  main:(){}.
  main(){
    try{
      As = taskManager([pinger(10)])
    } catch { _ do showMsg("huh?")
    }
  }

  pingA:(task[()])=>actor[pingBody].
  pingA(this) => valof{
    (Chnnl,Recv) = newSlot();
    png = pingBody{ ping() => valof{
	showMsg("We were pinged");
	valis .pong
      }. pomg() => unreachable };
    try{
      subTask(this,actorHead(Chnnl,png));
    } catch { _ do { showMsg("Ping was canceled") } };

    valis .actor(this,Recv)
  }

  pinger:(integer)=>(task[()])=>().
  pinger(K) => (this) => valof{
    showMsg("starting pinger $(K)");
    Count := K;

    showMsg("spawning ping agent");
    pingee = pingA(this);

    try{
      while Count!>0 do{
	pause();
	Count := Count! -1;

	_tell(pingee,(B:pingBody){B.ping()});
      };
    } catch {
      | .canceled do {
	showMsg("$(K) shutting down");
      }
    };
    showMsg("terminating");
    valis ()
  }
}
