test.tm{
  import star.
  import star.script.

  -- Test task expressions

  tsk ::= tsk(()=>result[(),()]).

  Tg = tag().

  QQ : ref qc[(())=>>result[(),()]].
  QQ = ref qc([],[]).

  yield:()=>result[(),()].
  yield() => do{
    _ .= (Tg cut K in yieldHandler(K));
    valis ()
  }

  yieldHandler:((())=>>result[(),()]) => result[(),()].
  yieldHandler(K) => do{
    (N,Qs) ^= _hdtl(append(QQ!,K));
    QQ := Qs;
    N.(())
  }

  spawnTask(tsk(F)) => do{
    valis (Tg cut K in createHandler(K,F))
  }

  createHandler:((())=>>result[(),()],()=>result[(),()])=>result[(),()].
  createHandler(K,F) => do{
    (N,Qs) ^= _hdtl(append(append(QQ!,contFromFun(Tg,(_)=>F())),K));
    QQ := Qs;
    N.(())
  }

  join:()=>result[(),()].
  join() => do{
    _ .= (Tg cut K in joinHandler(K));
    valis () -- never coming here
  }

  joinHandler:((())=>>result[(),()]) => result[(),()].
  joinHandler(K) => do{
    schedule()
  }

  schedule:() => result[(),()].
  schedule()=>do{
    if ~isEmpty(QQ!) then{
      (N,Qs) ^= _hdtl(QQ!);
      QQ := Qs;
      N.(())
    };
    valis ()
  }

  contFromFun:all x,y ~~ (tag[x,y],(x)=>y) => ((x)=>>y).
  contFromFun(T,F) => _fun2cont(T,F).

  drainQ()=>do{
    while (N,Qs) ^= _hdtl(QQ!) do {
      QQ := Qs;
      N.(())
    };
    valis ()
  }

  eventLoop:(cons[(string,tsk)])=>result[(),()].
  eventLoop(Tsks)=>do{
    Tg prompt {
      for (N,T) in Tsks do{
	logMsg("creating task $(N)");
	spawnTask(T);
      };
    };
    drainQ();
    valis ()
  }

  countDown:(integer)=>tsk.
  countDown(0) => tsk(()=>do{valis ()}).
  countDown(Ix) => tsk(()=>do{
      logMsg("Count $(Ix)");
      spawnTask(countDown(Ix-1));
      yield();
      logMsg("Counted $(Ix)")
    }).

  main:()=>action[(),()].
  main() => action{
    eventLoop([("count",countDown(1000))])
  }
}
