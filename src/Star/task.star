star.task{
  import star.core.
  import star.action.
  import star.iterable.
  import star.monad.
  import star.coerce.
  
  public all e,a ~~ task[e,a] ::=
    private _task(()=>taskEvalState[e,a]).


  all e,a,b ~~ taskEvalState[e,a] ::= taskDone(a) |
    taskContinue(task[e,a]) |
    taskWait(action[e,b],(b)=>task[e,a]) |
    taskFailure(e).
  
  taskWaitResult[e,a] ::= .taskSleep | taskMicroSleep(task[e,a]).

  public implementation execution[task] => let{.
    taskStep:all e,a,b ~~ (task[e,a],(a)=>task[e,b])=>task[e,b].
    taskStep(_task(B),N) =>
      _task(()=>case B() in {
	  taskDone(X) => taskContinue(N(X)).
	  taskContinue(T) => taskContinue(taskStep(T,N)).
	  taskWait(Start,More) => taskWait(Start,
	    (C)=>taskStep(More(C),N)).
	  taskFailure(E) => taskFailure(E).
	}).


    taskHandle:all e,f,a,b ~~ (task[e,a],(e)=>task[f,a])=>task[f,a].
    taskHandle(_task(B),H) =>
      _task(()=>
	  case B() in {
	    taskDone(X) => taskDone(X).
	    taskContinue(T) => taskContinue(taskHandle(T,H)).
--	    taskWait(Start,More) => taskWait(Start, (C)=>taskHandle(More(C),H)).
	    taskFailure(E) => taskContinue(H(E))
	  }).

    doStep:all e,a,b,f ~~ ((task[e,a])=>(),task[e,a])=>option[a].
    doStep(resume,_task(A)) => valof action{
      curr .= ref A;
      dun .= ref .false;
      res .= ref .none;
      while ~dun! do{
	case (curr!)() in {
	  taskDone(R) => {
	    dun := .true;
	    res := some(R)
	  }
	  taskContinue(_task(C)) => {
	    curr := C
	  }
	  taskWait(Start,K) => {
	    doWait(resume,Start,K);
	    dun := .true
	  }
	}
      };
      valis res!
    }

    doWait:all e,a,b ~~ ((task[e,a])=>action[e,a],((a)=>action[e,a])=>action[e,a],
      (a)=>task[e,b])=>action[e,a].
    doWait(resume,start,k) =>
      start((V)=>do{
	  resume(k(v))
	}).
 .} in {
    _valis(X) => _task(()=>taskDone(X)).
    _sequence = taskStep.
    _catch = taskHandle.
    _raise(E) => _task(()=>taskFailure(E)).
    _valof(T) => executeTask(T).
  }
}
