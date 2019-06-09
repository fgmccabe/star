star.task{
  import star.core.
  import star.action.
  import star.iterable.
  import star.monad.
  import star.coerce.
  import star.thread.
  
  public all e,a ~~ task[e,a] ::=
    private _task(()=>taskEvalState[e,a]).

  private taskEvalState[e,a] ::=
    taskDone(a) |
      taskContinue(task[e,a]) |
      taskWait(all b ~~ (task[e,b])=>taskWaitResult[e,a]) |
      taskFailure(e).

  private taskWaitResult[e,a] ::= taskSleep | taskMicroSleep(task[e,a]).

  public implementation all e ~~ execution[task[e]->>e] => let{
    taskStep(_task(B),N) => let{
      takeStep(taskDone(X)) => taskContinue(N(X)).
      taskStep(taskContinue(B2)) => taskContinue(taskStep(B2,F)).
      taskStep(taskWait(W)) => taskWait(()=>taskStep(W(),N)).
      taskStep(Fl) default => Fl.
    } in _task(()=>takeStep(B())).

    taskHandle(_task(B),H) => let{
      respond(taskFailure(E)) => taskContinue(H(E)).
      respond(taskContinue(B2)) => taskContinue(taskHandle(B2,H)).
      respond(taskWait(W)) => taskWait(()=>taskHandle()).
      respond(taskDone(X)) => taskDone(X).
    } in _task(()=>respond(B())).
  } in {
    _valis(X) => _task(()=>taskDone(X)).
    _sequence = taskStep.
    _handle = taskHandle.
    _raise = _task(()=>taskFailure(E)).
    _perform(T) => executeTask(T).
  }
}
