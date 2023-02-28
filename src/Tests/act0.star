test.act0{
  import star.
  import star.actor.
  import star.mbox.
  import star.script.

  -- First test of actors

  pingPong ::= .ping | .pong.

  implementation display[pingPong] => {
    disp(.ping) => "ping".
    disp(.pong) => "pong".
  }

  -- ping actor, replies with a pong for every ping, and vice versa

  pingBody::=pingBody{
    ping:()=>pingPong.
    pong:()=>pingPong
  }

  implementation sa[pingBody->>pingBody] => {
    _query(B,Q) => Q(B).
    _tell(B,Q) => Q(B)
  }

  main:()=>().
  main() => valof{
    png = pingBody{
      ping() => .pong.
      pong() => .ping.
    };

    try{
      Rs = _query(png,(P:pingBody)=>P.ping());
    
      logMsg("$(Rs)");
    } catch { _ => logMsg("huh?")
    };
    valis ()
  }

  -- pingA:(task[()])=>actor[pingBody].
  -- pingA(this) => valof{
  --   Chnnl = newChannel();
  --   valis .actor(spawn(actorHead(Chnnl,pingBody{
  -- 	ping() => .pong
  -- 	  })),Chnnl)
  -- }

  -- pongBody::=pongBody{
  --   pong:()=>pingPong
  -- }

  -- pongA:(task[()])=>actor[pongBody].
  -- pongA(this) => valof{
  --   Chnnl = newChannel();
  --   valis .actor(spawn(actorHead(Chnnl,pongBody{
  -- 	pong() => .ping
  -- 	  })),Chnnl)
  -- }

  -- main:()=>().
  -- main() => valof{
  --   AChnnl = newChannel();
  --   N = nursery([]);
  --   logMsg("final result $(N)");
  --   valis ()
  -- }
}

