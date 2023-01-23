star.structured.conn{
  import star.

  public all e ~~ task[e] ~> fiber[resumeProtocol,suspendProtocol[e]].

  public all d,e ~~ channel[d,e] <~ {}.

  channel:all d,e ~~ (ref channelState[d,e]) <=> channel[d,e].

  channelState[d,e] ::= .quiescent
  | .hasData(d)
  | .waiting(task[e]).

  public newChannel:all d,e ~~ () => channel[d,e].
  newChannel() => .channel(ref .quiescent).

  public all e ~~ suspendProtocol[e] ::= .yield_ |
  .blocked(()=>boolean) |
  .wake(task[e]) |
  .result(e) |
  .fork((task[e])=>e) |
  .identify(task[e]) |
  .retired_.

  public resumeProtocol ::= .go_ahead | .shut_down_.

  public post:all e,d ~~ (task[e],d,channel[d,e])=>() raises exception.
  post(T,D,Ch where .channel(St).=Ch) => valof{
    case St! in {
      .hasData(_) => {
	case T suspend .blocked(()=>.hasData(_).=St!) in {
	  .go_ahead => valis post(T,D,Ch)
	  | .shut_down_ => raise .canceled
	}
      }
      | .quiescent => {
	St := .hasData(D);
	case T suspend .yield_ in {
	  .go_ahead => valis ()
	  | .shut_down_ => raise .canceled
	}
      }
      | .waiting(RR) => {
	St := .hasData(D);
	case T suspend .wake(RR) in {
	  .go_ahead => valis ()
	  | .shut_down_ => raise .canceled
	}
      }
    }
  }

  public collect:all d,e ~~ (task[e],channel[d,e]) => d raises exception.
  collect(T,Ch where .channel(St).=Ch) => valof{
    case St! in {
      .hasData(D) => {
	St := .quiescent;
	case T suspend .yield_ in {
	  .go_ahead => valis D
	  | .shut_down_ => raise .canceled
	}
      }.
      .quiescent => {
	St := .waiting(T);
	case T suspend .blocked(()=> ~.hasData(_).=St!) in {
	  .go_ahead => valis collect(T,Ch)
	  | .shut_down_ => raise .canceled
	}
      }.
      .waiting(TT) => {
	case T suspend .blocked(()=> ~.hasData(_).=St!) in {
	  .go_ahead =>
	    valis collect(T,Ch)
	  | .shut_down_ => raise .canceled
	}
      }
    }
  }
  
  public nursery:all e ~~ (cons[task[e]]) => e.
  nursery(Ts) => _spawn((This) => valof{
      Q := Ts::qc[task[e]];
      BlockQ := ([]:cons[(()=>boolean,task[e])]);

      while .true do{
	while ~isEmpty(Q!) do{
	  if [T,..Rs] .= Q! then{
	    Q := Rs;
	    case T resume .go_ahead in {
	      .yield_ => { Q:=Q!++[T] }
	      | .result(Rslt) => {
		while [C,..Cs] .= Q! do{
		  Q := Cs;
		  _ = C resume .shut_down_;
		};
		
		valis Rslt
	      }
	      | .retired_ => {}
	      | .fork(F) => {
		case _spawn((Tsk) => valof{
		    case Tsk suspend .identify(Tsk) in {
		      .go_ahead => {
		        Tsk retire .result(F(Tsk))
		      }
		      | .shut_down_ => {}
		    };
		    Tsk retire .retired_
		  }) in {
		  .identify(Tsk) => { Q := Q!++[Tsk]}
		  };
		Q := Q!++[T];
	      }
	      | .blocked(P) => {
	        BlockQ := [(P,T),..BlockQ!]
	      }
	      | .wake(W) => {
		Q := [W,T]++Q!;
		BlockQ := removeT(BlockQ!,W);
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

  removeT(Q,W) => {(P,Ts) | (P,Ts) in Q && ~W==Ts}.

  public canceled : () <=> exception.

  public pause:all e ~~ this |= task[e] |: () => () raises exception.
  pause() => valof{
    case this suspend .yield_ in {
      .go_ahead => valis ()
      | .shut_down_ => raise .canceled
    }
  }

  public spawn:all e ~~ this |= task[e] |: ((task[e])=>e) => () raises exception.
  spawn(F) => valof{
    case this suspend .fork(F) in {
      .go_ahead => valis ()
      | .shut_down_ => raise .canceled
    }
  }

  implementation all e ~~ display[suspendProtocol[e]] => {
    disp(.yield_) => "yield".
    disp(.blocked(B)) => "blocked $(B())".
    disp(.wake(T)) => "wake #(_stringOf(T,2))".
    disp(.result(e)) => "result #(_stringOf(e,2))".
    disp(.fork(F)) => "fork #(_stringOf(F,2))".
    disp(.identify(T)) => "identify #(_stringOf(T,2))".
    disp(.retired_) => "retired"
  }

}
