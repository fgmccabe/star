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
    
      showMsg("$(Rs)");

      assert Rs==.pong;
    } catch mboxException in { _ => showMsg("huh?")
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
}

