star.actor{
  import star.
  import star.mbox.

  public contract all a,i ~~ sa[a->>i] ::= {
    _query:all x ~~ (a,(i)=>x) => x throws mboxException.
    _tell:(a,(i)=>()) => () throws mboxException.
  }

  public actorProtocol[i] ::=
    exists r ~~ .query((i)=>r, receiver[r])
    | .tell((i)=>()).

  public actorHead:all i ~~ (emitter[actorProtocol[i]],i) => taskFun[()].
  actorHead(mBox,body) => (this) => valof{
    while .true do{
      try{
	case collectMsg(mBox) in {
	  | .query(Q,Reply) do { postMsg(Q(body),Reply) }
	  | .tell(A) do {
	    A(body);
	  }
	}
      } catch { _ do { showMsg("Problem in actor"); valis () }}
    };
    valis ()
  }

  public actor[i] ::= .actor(task[()],receiver[actorProtocol[i]]).

  public implementation all i ~~ sa[actor[i]->>i] => {
    _query(.actor(this,Ch),Q) => valof{
      (P,R) = newSlot();
      postMsg(.query(Q,R),Ch);
      valis collectMsg(P)
    }
    _tell(.actor(this,Ch),A) => valof{
      postMsg(.tell(A),Ch);
      valis ()
    }
  }
}
	
	
