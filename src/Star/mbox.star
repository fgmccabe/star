star.mbox{
  import star.

  public all e ~~ task[e] ~> fiber[resumeProtocol,suspendProtocol[e]].

  public all e ~~ taskFun[e] ~> ((task[e])=>e).

  public all d ~~ channel[d] <~ {}.

  channel:all d ~~ (ref channelState[d]) <=> channel[d].

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

  public post:all e,d ~~ (task[e],d,channel[d])=>() raises exception.
  post(T,D,Ch where .channel(St).=Ch) => valof{
    case St! in {
      .hasData(_) => {
	case _suspend(T,.blocked(()=>.hasData(_).=St!)) in {
	  .go_ahead => valis post(T,D,Ch)
	  | .shut_down_ => raise .canceled
	}
      }
      | .quiescent => {
	St := .hasData(D);
	case _suspend(T,.yield_) in {
	  .go_ahead => valis ()
	  | .shut_down_ => raise .canceled
	}
      }
    }
  }

  public collect:all d,e ~~ (task[e],channel[d]) => d raises exception.
  collect(T,Ch where .channel(St).=Ch) => valof{
    case St! in {
      .hasData(D) => {
	St := .quiescent;
	case _suspend(T,.yield_) in {
	  .go_ahead => valis D
	  | .shut_down_ => raise .canceled
	}
      }
      | .quiescent => {
	case _suspend(T,.blocked(()=> ~.hasData(_).=St!)) in {
	  .go_ahead => valis collect(T,Ch)
	  | .shut_down_ => raise .canceled
	}
      }
    }
  }
  
  spawnTask:all e ~~ ((task[e])=>e) => task[e].
  spawnTask(F) => case _spawn((Tsk) => valof{
      case _suspend(Tsk,.identify(Tsk)) in {
	.go_ahead => {
	  _retire(Tsk,.result(F(Tsk)))
	}
	| .shut_down_ => {}
      };
      _retire(Tsk,.retired_)
    }) in {
    .identify(Tsk) => Tsk
    }.

  public nursery:all e ~~ (cons[taskFun[e]]) => e.
  nursery(Ts) => _spawn((This) => valof{
      Q := (Ts//spawnTask)::qc[task[e]];
      BlockQ := ([]:cons[(()=>boolean,task[e])]);

      while .true do{
	while ~isEmpty(Q!) do{
--	  logMsg("Q ~ $([|Q!|])");	  
	  if [T,..Rs] .= Q! then{
	    Q := Rs;
--	    logMsg("resuming");
--	    _ins_debug();
	    case _resume(T,.go_ahead) in {
	      .yield_ => { Q:=Q!++[T]; /*logMsg("yielding");*/ }
	      | .result(Rslt) => {
		while [C,..Cs] .= Q! do{
		  Q := Cs;
		  _ = _resume(C,.shut_down_);
		};
		
		valis Rslt
	      }
	      | .retired_ => {}
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
	}
      }
    }).

  testBlocked([],BQ,Ws) => (BQ,Ws).
  testBlocked([(P,T),..Q],BQ,Ws) where ~P() =>
    testBlocked(Q,BQ,[T,..Ws]).
  testBlocked([(P,T),..Q],BQ,Ws) =>
    testBlocked(Q,[(P,T),..BQ],Ws).

  public canceled : () <=> exception.

  public pause:all e ~~ this |= task[e] |: () => () raises exception.
  pause() => valof{
    case _suspend(this,.yield_) in {
      .go_ahead => valis ()
      | .shut_down_ => raise .canceled
    }
  }

  public spawn:all e ~~ this |= task[e] |: (taskFun[e]) => () raises exception.
  spawn(F) => valof{
    case _suspend(this,.fork(F)) in {
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
