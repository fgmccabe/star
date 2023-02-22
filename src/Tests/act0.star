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

  -- ping actor, replies with a pong for every ping

  pingBody::=pingBody{
    ping:()=>pingPong
  }

  pingA:(task[()])=>actor[pingPong].
  pingA(Th) => valof{
    Chnnl = newChannel();
    valis .actor(spawnTask(actorHead(Chnnl,pingBody{
	ping() => .pong
	  })),Chnnl)
  }
}

