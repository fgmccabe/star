star.structured.conn{
  import star.

  public all e ~~ task[e] ~> fiber[resumeProtocol,suspendProtocol[e]].

  public all e ~~ channel[e] ::= .quiescent
  | .hasData(e)
  | .waiting(exists e ~~ task[e]).

  public all e ~~ suspendProtocol[e] ::= .yield_ |
  .blocked(()=>boolean) |
  .wake(exists r ~~ task[r]) |
  .result(e) |
  .fork((task[e])=>e) |
  .identify(task[e]) |
  .retired_.

  public resumeProtocol ::= .go_ahead | .shut_down_.

  post:all e,d ~~ (task[e],d,ref channel[d])=>().
  post(T,D,Ch) => valof{
    case Ch! in {
      .hasData(_) => {
	case T suspend .blocked(()=>.hasData(_).=Ch!) in {
	  .go_ahead => valis post(T,D,Ch)
	}
      }.
      .quiescent => {
	Ch := .hasData(D);
	case T suspend .yield_ in {
	  .go_ahead => valis ()
	}
      }.
      .waiting(RR) => {
	Ch := .hasData(D);
	case T suspend .wake(RR) in {
	  .go_ahead =>
	    valis ()
	}
      }
    }
  }

  collect:all e ~~ (task[e],ref channel[e]) => e.
  collect(T,Ch) => valof{
    case Ch! in {
      .hasData(D) => {
	Ch := .quiescent;
	case T suspend .yield_ in {
	  .go_ahead =>
	    valis D
	}
      }.
      .quiescent => {
	Ch := .waiting(T);
	case T suspend .blocked(()=>~ .hasData(_).=Ch!) in {
	  .go_ahead =>
	    valis collect(T,Ch)
	}
      }.
      .waiting(TT) => {
	case T suspend .blocked(()=>~ .hasData(_).=Ch!) in {
	  .go_ahead =>
	    valis collect(T,Ch)
	}
      }
    }
  }
  

  public nursery:all e ~~ (cons[task[e]]) => e.
  nursery(Ts) => _spawn((This) => valof{
      Q := Ts::qc[task[e]];
      BlockQ := ([]:qc[(()=>boolean,task[e])]);

      while .true do{
	while ~isEmpty(Q!) do{
--	  logMsg("Q has $([|Q!|]) elements");
	  
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
	    }
	  };
	  (BQ,Wts) = testBlocked(BlockQ!,[],[]);
	  Q := Q!++Wts;
	  BlockQ := BQ;
	};
	
	if ~isEmpty(BlockQ!) then{
	  (BQ,Wts) = testBlocked(BlockQ!,[],[]);
	  BlockQ := BQ;
	  Q := Wts
	}
      }
    }).

  testBlocked([],BQ,Ws) => (BQ,Ws).
  testBlocked([(P,T),..Q],BQ,Ws) where P() =>
    testBlocked(Q,BQ,[T,..Ws]).
  testBlocked([(P,T),..Q],BQ,Ws) =>
    testBlocked(Q,[(P,T),..BQ],Ws).

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
  
}
