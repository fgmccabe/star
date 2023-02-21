star.actor{
  import star.
  import star.mbox.

  public contract all a,i ~~ sa[a->>i] ::= {
    _query:all x ~~ (a,(i)=>x) => x raises exception.
    _tell:(a,(i)=>()) => () raises exception.
  }

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

  actor[i] ::= .actor(task[i],channel[actorProtocol[i]]).

  implementation all i ~~ sa[actor[i]->>i] => {
    _query(.actor(T,Ch),Q) => valof{
      R = newChannel();
      post(T,query{q=Q. resp=R},Ch);
      valis collect(T,R)
    }
    _tell(.actor(T,Ch),A) => valof{
      post(T,.tell(A),Ch);
      valis ()
    }
  }
}
	
	
