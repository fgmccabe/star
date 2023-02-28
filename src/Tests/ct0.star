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
	if Count! % 3 == 0 then{
	  Cnt = Count!;
	  logMsg("spawning sub-task");
	  spawn((Tsk)=>valof{
	      try{
		logMsg("We were spawned $(Cnt)");
		case _suspend(Tsk,.yield_) in {
		  .go_ahead => {}
		  | .shut_down_ => raise .canceled
		};
		logMsg("After 1 pause $(Cnt)");
		case _suspend(Tsk,.yield_) in {
		  .go_ahead => {}
		  | .shut_down_ => raise .canceled
		};
		logMsg("After 2 pauses $(Cnt)");
		_retire(Tsk,.blocked(()=>.false))
	      } catch {
		_ => {
		  logMsg("we were canceled");
		  _retire(Tsk,.retired_)
		}
	      }
	    });
	};
	logMsg("$(K) moving along, $(Count!) rounds left");
      }
    } catch { .canceled => {
	logMsg("$(K) shutting down");
      }
    };
    valis ()
  }.
  
  main:()=>().
  main() => valof{
    Rs = nursery([tt(10),tt(20)]);
    logMsg("final result $(Rs)");
    valis ()
  }
}
