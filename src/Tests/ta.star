test.ta{
  import star.
  import star.script.

  -- Test task expressions

  tsk ::= tsk(()=>result[(),()]).

  Tg = tag().

  QQ : ref qc[(())=>>result[(),()]].
  QQ = ref qc([],[]).

  yield:(string)=>result[(),()].
  yield(Nm) => do{
    logMsg("yield #(Nm)");
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
--    logMsg("after spawn $(size(QQ!)) entries");
    N.(())
  }

  join:(string)=>result[(),()].
  join(Nm) => do{
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

  threadMain:(string)=>tsk.
  threadMain(T) => tsk(() => do{
      logMsg("A$(T)");
      yield(T);
      logMsg("B$(T)");
      join(T)
    })

  main:()=>action[(),()].
  main() => action{
    eventLoop([("alpha",threadMain("alpha")),
    	("beta",threadMain("beta")),
    	("gamma",threadMain("gamma"))]);
    logMsg("Q is $(size(QQ!))");
  }
}