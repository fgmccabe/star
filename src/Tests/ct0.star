test.ct0{
  -- Some simple nursery tests

  import star.
  import star.script.
  import star.mbox.

  tt:(integer)=>(task[()])=>().
  tt(K) => (this) => valof{
    logMsg("starting $(K)");
    Count := K;
    try{
      while Count!>0 do{
	pause();
	Count := Count! -1;
	if dividesBy(Count!,3) then{
	  Cnt = Count!;
	  logMsg("spawning sub-task");
	  case (Tsk spawn valof{
	    try{
	      logMsg("We were spawned $(Cnt)");
	      case Tsk suspend .yield_ in {
		.go_ahead => {}
		| .shut_down_ => raise .canceled
	      };
	      logMsg("After 1 pause $(Cnt)");
	      case Tsk suspend .yield_ in {
		.go_ahead => {}
		| .shut_down_ => raise .canceled
	      };
	      logMsg("After 2 pauses $(Cnt)");
	      Tsk retire .blocked(()=>.false)
	    } catch mboxException in {
	      _ => {
		logMsg("we were canceled");
		Tsk retire .retired_
	      }
	    }
	    }) in {
	    .yield_ => {}
	    };
	};
	logMsg("moving along, $(Count!) rounds left ");
      };
      logMsg("end of try");
    } catch mboxException in { .canceled => {
	logMsg("$(K) shutting down");
      }
    };
    logMsg("terminating");
    valis ()
  }.

  dividesBy(X,Y) => (try X%Y==0 catch exception in { _ => .false}).
  
  main:()=>().
  main() => valof{
    try{
      Rs = nursery([tt(10),tt(20)]);
      logMsg("final result $(Rs)");
    } catch mboxException in {
      E => logMsg(disp(E))
    };
    valis ()
  }
}
