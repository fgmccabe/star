test.ta{
  import star.
  import star.script.

  -- Test task expressions

  all e,a ~~ tsk[e,a] ::= _tsk{
    cont:()=>>result[e,a]
  }.

  TaskTg = tag().

  TskQ : ref qc[tsk[(),()]].
  TskQ = ref qc([],[]).

  yield:()=>().
  yield() => TaskTg cut K in yieldHandler(_tsk{cont=K}).

  yieldHandler:((())=>>result[(),()]) => result[(),()].
  yieldHandler(K) => do{
    (N,Qs) ^= _hdtl(append(TskQ!,K));
    TskQ := Qs;
    (N.cont).(())
  }

  spawnTask(F) =>
    TaskTg cut K in createHandler(K,F).

  taskFromFun(T,F) => _tsk{cont=_fun2cont(T,F)}.

  createHandler(K,F) => do{
    (N,Qs) ^= _hdtl(append(append(TskQ!,taskFromFun(TaskTg,F)),K));
    TskQ := Qs;
    N.(())
  }

  drainQ:()=>result[(),()].
  drainQ()=>do{
    while (N,Qs) ^= _hdtl(TskQ!) do {
      TskQ := Qs;
      (N.cont).(())
    };
    valis ()
  }

  threadMain:(string)=>(())=>result[(),()].
  threadMain(T) => (_) => do{
    logMsg("A$(T)");
    _.=yield();
    logMsg("B$(T)");
    _ .= yield();
    valis()
  }

  main:()=>action[(),()].
  main() => action{
    TaskTg prompt {    
      _ .= spawnTask(threadMain("alpha"));
      _ .= spawnTask(threadMain("beta"));
      drainQ()
    }
  }
}
