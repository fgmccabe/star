star.structured.conn{
  import star.

  public all e ~~ task[e] ~> fiber[resumeProtocol,suspendProtocol[e]].

  public all e ~~ channel[e] ::= .quiescent
  | .hasData(e)
  | .waiting(exists e ~~ task[e]).

  public all e ~~ suspendProtocol[e] ::= .yield_ |
  .blocked |
  .wake(exists r ~~ task[r]) |
  .result(e) |
  .fork(task[e]).

  public resumeProtocol ::= .go_ahead | .shut_down_.

  post:all e,d ~~ (task[e],d,ref channel[d])=>().
  post(T,D,Ch) => valof{
    case Ch! in {
      .hasData(_) => {
	case T suspend .blocked in {
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
	case T suspend .blocked in {
	  .go_ahead =>
	    valis collect(T,Ch)
	}
      }.
      .waiting(TT) => {
	case T suspend .blocked in {
	  .go_ahead =>
	    valis collect(T,Ch)
	}
      }
    }
  }
  

  public nursery:all e ~~ (cons[task[e]]) => e.
  nursery(Ts) => _spawn((This) => valof{
      Q := Ts::qc[task[e]];

      while .true do{
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
	    | .fork(F) => {
	      Q := Q!++[T,F]
	    }
	  }
	}
      }
    }).
}
