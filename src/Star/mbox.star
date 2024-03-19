star.mbox{
  import star.

  public all e ~~ suspendProtocol[e] ::= .yield_ |
  .blocked(()=>boolean) |
  .result(e) |
  .fork(taskFun[e]) |
  .requestIO(ioHandle,()=>boolean) |
  .identify(task[e]) |
  .retired_.

  public resumeProtocol ::= .go_ahead | .shut_down_.

  public mboxException ::= .deadlock | .canceled.

  public all e ~~ task[e] ~> resumeProtocol=>>suspendProtocol[e].

  public all e ~~ taskFun[e] ~> ((task[e])=>e).

  channelState[d] ::= .quiescent
  | .hasData(d).

  public all d ~~ emitter[d] ::=
    .emitter(ref channelState[d]).

  public all d ~~ receiver[d] ::=
    .receiver(ref channelState[d]).

  public all d ~~ slot[d] ~> (emitter[d],receiver[d]).

  public newSlot:all d ~~ ()=>slot[d].
  newSlot() => valof{
    Ch := .quiescent;
    valis (.emitter(Ch),.receiver(Ch))
  }

  public post:all e,d ~~ (this:task[e]), raises mboxException |:
    (d,receiver[d])=>().
  post(D,Ch where .receiver(St).=Ch) => valof{
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

  public collect:all d,e ~~ (this:task[e]), raises mboxException |:(emitter[d]) => d.
  collect(Ch where .emitter(St).=Ch) => valof{
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

  public implementation display[mboxException] => {
    disp(.canceled) => "canceled".
    disp(.deadlock) => "deadlocked"
  }

  spawnTask:all e ~~ (taskFun[e]) => task[e].
  spawnTask(F) => case (Tsk spawn valof{
    case Tsk suspend .identify(Tsk) in {
	.go_ahead => { Tsk suspend .result(F(Tsk))}
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
    IoQ := ([]:cons[(ioHandle,()=>boolean,task[e])]);

    while ~isEmpty(Q!) || ~isEmpty(BlockQ!) || ~isEmpty(IoQ!) do{
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
		C resume .shut_down_;
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
	    | .requestIO(Io,Rdy) => {
	      IoQ := [(Io,Rdy,T),..IoQ!];
	    }
	  }
	}
      };

      if _waitIo(IoQ!,-1) then{
	(IQ,Wts) = testIoQ(IoQ!,[],[]);
	IoQ := IQ;
	Q := Wts++Q!
      };

      if ~isEmpty(BlockQ!) then{ -- Poll the blocked queue
	(BQ,Wts) = testBlocked(BlockQ!,[],[]);
	BlockQ := BQ;
	Q := Wts
      }
    };
    raise .canceled
  }

  testBlocked:all y ~~ (cons[(()=>boolean,y)],cons[(()=>boolean,y)],qc[y])=>
    (cons[(()=>boolean,y)],qc[y]).
  testBlocked([],BQ,Ws) => (BQ,Ws).
  testBlocked([(P,T),..Q],BQ,Ws) where ~P() =>
    testBlocked(Q,BQ,[T,..Ws]).
  testBlocked([(P,T),..Q],BQ,Ws) =>
    testBlocked(Q,[(P,T),..BQ],Ws).

  testIoQ:all y ~~ (cons[(ioHandle,()=>boolean,y)],cons[(ioHandle,()=>boolean,y)],qc[y])=>
    (cons[(ioHandle,(()=>boolean),y)],qc[y]).
  testIoQ([],BQ,Ws) => (BQ,Ws).
  testIoQ([(Io,P,T),..Q],BQ,Ws) where ~P() =>
    testIoQ(Q,BQ,[T,..Ws]).
  testIoQ([(Io,P,T),..Q],BQ,Ws) =>
    testIoQ(Q,[(Io,P,T),..BQ],Ws).
  
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

  public waitfor:all k,e,t ~~ (this:task[t]), raises e |: (future[k,e])=>k.
  waitfor(Ft) => valof{
    case this suspend .blocked(()=>~_futureIsResolved(Ft)) in {
      .go_ahead => {
	if _futureIsResolved(Ft) then{
	  valis _futureVal(Ft)
	} else
	this retire .retired_
      }
      _ =>
	this retire .retired_
    }
  }

  -- Create a future from a user defined function
  public tsk:all k,e,t ~~
  (task[t],((this:task[t]), raises e|:()=>k)) => future[k,e].
  tsk(sched,TFn) => valof{
    C = ref .neither;
    sched suspend .fork((this)=>valof{
	try{
	  C := .either(TFn()); -- this marks the future as resolved
	  this retire .retired_
	} catch e in {
	  Ex => {
	    C := .other(Ex);
	    this retire .retired_
	  }
	};
	this retire .retired_
      });
    valis _cell_future(C)
  }
}
