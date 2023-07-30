star.mbox{
  import star.

  public all e ~~ task[e] ~> resumeProtocol=>>suspendProtocol[e].

  public all e ~~ taskFun[e] ~> ((task[e])=>()).

  public all d ~~ channel[d] ::=
    .channel(ref channelState[d]).

  channelState[d] ::= .quiescent
  | .hasData(d).

  public newChannel:all d ~~ () => channel[d].
  newChannel() => .channel(ref .quiescent).

  public all e ~~ suspendProtocol[e] ::= .yield_ |
  .blocked(()=>boolean) |
  .result(e) |
  .fork(taskFun[e]) |
  .identify(task[e]) |
  .retired_.

  public resumeProtocol ::= .go_ahead | .shut_down_.

  public mboxException ::= .deadlock | .canceled.

  public implementation display[mboxException] => {
    disp(.canceled) => "canceled".
    disp(.deadlock) => "deadlocked"
  }

  public post:all e,d ~~ (this:task[e]), raises mboxException |: (d,channel[d])=>().
  post(D,Ch where .channel(St).=Ch) => valof{
    case St! in {
      .hasData(_) => {
	case this suspend .blocked(()=>.hasData(_).=St!) in {
	  .go_ahead => valis post(D,Ch)
	  | .shut_down_ => raise .canceled
	}
      }
      | .quiescent => {
	St := .hasData(D);
	case this suspend .yield_ in {
	  .go_ahead => valis ()
	  | .shut_down_ => raise .canceled
	}
      }
    }
  }

  public collect:all d,e ~~ (this:task[e]), raises mboxException |:(channel[d]) => d.
  collect(Ch where .channel(St).=Ch) => valof{
    case St! in {
      .hasData(D) => {
	St := .quiescent;
	case this suspend .yield_ in {
	  .go_ahead => valis D
	  | .shut_down_ => raise .canceled
	}
      }
      | .quiescent => {
	case this suspend .blocked(()=> ~.hasData(_).=St!) in {
	  .go_ahead => valis collect(Ch)
	  | .shut_down_ => raise .canceled
	}
      }
    }
  }
  
  spawnTask:all e ~~ (taskFun[e]) => task[e].
  spawnTask(F) => case (Tsk spawn valof{
    case Tsk suspend .identify(Tsk) in {
	.go_ahead => {
	  F(Tsk);
	}
	| .shut_down_ => {}
      };
      Tsk retire .retired_
    }) in {
    .identify(Tsk) => Tsk
    }.

  public nursery:all e ~~ raises mboxException |: (cons[taskFun[e]]) => e.
  nursery(Ts) => valof{
    Q := (Ts//spawnTask)::qc[task[e]];
    BlockQ := ([]:cons[(()=>boolean,task[e])]);

    while .true do{
      while ~isEmpty(Q!) do{
	if [T,..Rs] .= Q! then{
	  Q := Rs;
	  case T resume .go_ahead in {
	    .yield_ => {
	      Q:=Q!++[T];
	    }
	    | .result(Rslt) => {
	      while [C,..Cs] .= Q! do{
		Q := Cs;
		_ = C resume .shut_down_;
	      };
		
	      valis Rslt
	    }
	    | .retired_ => { }
	    | .fork(F) => {
	      Tsk = spawnTask(F);
	      Q := Q! ++ [Tsk,T];
	    }
	    | .blocked(P) => {
	      BlockQ := [(P,T),..BlockQ!]
	    }
	  }
	};
      };
	
      if ~isEmpty(BlockQ!) then{
	(BQ,Wts) = testBlocked(BlockQ!,[],[]);
	BlockQ := BQ;
	Q := Wts
      };

      if isEmpty(Q!) then{
	raise .deadlock
      }
    }
  }

  testBlocked:all y ~~ (cons[(()=>boolean,y)],cons[(()=>boolean,y)],qc[y])=>
    (cons[(()=>boolean,y)],qc[y]).
  testBlocked([],BQ,Ws) => (BQ,Ws).
  testBlocked([(P,T),..Q],BQ,Ws) where ~P() =>
    testBlocked(Q,BQ,[T,..Ws]).
  testBlocked([(P,T),..Q],BQ,Ws) =>
    testBlocked(Q,[(P,T),..BQ],Ws).

  public pause:all e ~~ this |= task[e], raises mboxException |: () => ().
  pause() => valof{
    case this suspend .yield_ in {
      .go_ahead => valis ()
      | .shut_down_ => raise .canceled
    }
  }

  implementation all e ~~ display[suspendProtocol[e]] => {
    disp(.yield_) => "yield".
    disp(.blocked(B)) => "blocked $(B())".
    disp(.result(e)) => "result #(_stringOf(e,2))".
    disp(.fork(F)) => "fork #(_stringOf(F,2))".
    disp(.identify(T)) => "identify #(_stringOf(T,2))".
    disp(.retired_) => "retired"
  }

  implementation display[resumeProtocol] => {
    disp(.go_ahead) => "go ahead".
    disp(.shut_down_) => "shut down".
  }

  implementation all d ~~ display[d] |: display[channelState[d]] => {
    disp(.quiescent) => "quiescent".
    disp(.hasData(D)) => "hasData($(D))".
  }
}
