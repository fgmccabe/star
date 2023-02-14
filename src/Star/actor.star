star.actor{
  import star.
  import star.mbox.

  -- public contract all a,i ~~ sa[a->>i] ::= {
  --   _query:all x ~~ (a,(i)=>x) => x.
  --   _tell:(a,(i)=>()) => ().
  -- }

  actorProtocol[i] ::=
    exists r ~~ query{ q:(i)=>r. resp:channel[r]}
    | .tell((i)=>()).

  actorHead:all i ~~ (channel[actorProtocol[i]],i) => taskFun[i].
  actorHead(mBox,body) => (this) => valof{
    while .true do{
      try{
	case collect(this,mBox) in {
	  query{q=Q. resp=Reply. } => {
	    post(this,Q(body),Reply);
	  }
	  | .tell(A) => {
	    A(body);
	  }
	}
      } catch { (_) => logMsg("Problem in actor") }
    }
  }
}
	
	
