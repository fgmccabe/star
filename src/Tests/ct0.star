test.ct0{
  -- Some simple nursery tests

  import star.
  import star.assert.
  import star.mbox.

  tt:(integer)=>(task[()])=>().
  tt(K) => (this) => valof{
    showMsg("starting $(K)");
    Count := K;
    try{
      while Count!>0 do{
	pause();
	Count := Count! -1;
	if dividesBy(Count!,3) then{
	  Cnt = Count!;
	  showMsg("spawning sub-task");
	  subTask(this,
	    (Tsk)=>valof{
	      try{
		showMsg("We were spawned $(Cnt)");
		case _suspend(Tsk,.yield_) in {
		  | .go_ahead => {}
		  | .shut_down_ => raise .canceled
		};
		showMsg("After 1 pause $(Cnt)");
		case _suspend(Tsk,.yield_) in {
		  | .go_ahead => {}
		  | .shut_down_ => raise .canceled
		};
		showMsg("After 2 pauses $(Cnt)");
		_retire(Tsk,.blocked(()=>.false))
	      } catch mboxException in {
		| _ => {
		  showMsg("we were canceled");
		  _retire(Tsk,.retired_)
		}
	      }
	    });
	};
	showMsg("moving along, $(Count!) rounds left ");
      };
      showMsg("end of try");
    } catch mboxException in {
      | .canceled => {
	showMsg("$(K) shutting down");
      }
    };
    showMsg("terminating");
    valis ()
  }.

  dividesBy(X,Y) => (try X%Y==0 catch exception in { _ => .false}).
  
  main:()=>().
  main() => valof{
    try{
      Rs = nursery([tt(10),tt(20)]);
      showMsg("final result $(Rs)");
    } catch mboxException in {
      E => showMsg(disp(E))
    };
    valis ()
  }
}
