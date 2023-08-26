star.actor{
  import star.
  import star.mbox.

  public contract all a,i ~~ sa[a->>i] ::= {
    _query:all x ~~ raises mboxException |: (a,(i)=>x) => x.
    _tell:raises mboxException |: (a,(i)=>()) => ().
  }

  public actorProtocol[i] ::=
    exists r ~~ query{ q:(i)=>r. resp:channel[r]}
    | .tell((i)=>()).

  public actorHead:all i ~~ (channel[actorProtocol[i]],i) => taskFun[()].
  actorHead(mBox,body) => (this) => valof{
    while .true do{
      try{
	case collect(mBox) in {
	  query{q=Q. resp=Reply. } => {
	    post(Q(body),Reply);
	  }
	  | .tell(A) => {
	    A(body);
	  }
	}
      } catch mboxException in { (_) => { logMsg("Problem in actor"); valis () }}
    }
  }

  public actor[i] ::= .actor(task[()],channel[actorProtocol[i]]).

  public implementation all i ~~ sa[actor[i]->>i] => {
    _query(.actor(this,Ch),Q) => valof{
      R = newChannel();
      post(query{q=Q. resp=R},Ch);
      valis collect(R)
    }
    _tell(.actor(this,Ch),A) => valof{
      post(.tell(A),Ch);
      valis ()
    }
  }
}
	
	
