star.fiber{
  import star.core.
  
  public all e,a ~~ fiber[e,a] <~ {}.

  public all e ~~ task[e] ~> fiber[resumeProtocol,suspendProtocol[e]].

  public all e ~~ channel[e] ::= .quiescent |
    .hasData(e) |
    .waitingFor(exists e ~~ task[e]).

  public all e ~~ suspendProtocol[e] ::= .yield_ |
    .blocked |
    .wake(exists r ~~ task[r]) |
    .result(e).

  public resumeProtocol ::= .go_ahead.

  post:all e,d ~~ (task[e],d,ref channel[d])=>().
  post(T,D,Ch) => valof{
    case Ch! in {
      .hasData(_) => {
	T suspend .blocked in {
	  .go_ahead => post(T,D,Ch)
	}
      }.
      .quiescent => {
	Ch := .hasData(D);
	T suspend .yield_ in {
	  .go_ahead =>
	    valis ()
	}
      }.
      .waitingFor(RR) => {
	Ch := .hasData(D);
	T suspend wake(RR) in {
	  .go_ahead =>
	    valis ()
	}
      }
    }
  }

  collect:all d,e ~~ (task[e],ref channel[d]) => d.
  collect(T,Ch) => valof{
    case Ch! in {
      .hasData(D) => {
	Ch := .quiescent;
	T suspend .yield_ in {
	  .go_ahead =>
	    valis D
	}
      }.
      .quiescent => {
	Ch := waitingFor(T);
	T suspend .blocked in {
	  .go_ahead =>
	    valis collect(T,Ch)
	}
      }.
      .waitingFor(TT) => {
	T suspend .blocked in {
	  .go_ahead =>
	    valis collect(T,Ch)
	}
      }
    }
  }
}
