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
--    logMsg(_stackTrace());
    logMsg("yield #(Nm) with $(size(QQ!)) entries");
    _ .= (Tg cut K in yieldHandler(K));
    logMsg("coming back from yield to #(Nm)")
  }

  yieldHandler:((())=>>result[(),()]) => result[(),()].
  yieldHandler(K) => do{
    logMsg("handle yield");
    (N,Qs) ^= _hdtl(append(QQ!,K));
    logMsg("$(size(Qs)) remaining entries");
    QQ := Qs;
    N.(())
--    schedule()
  }

  spawnTask(tsk(F)) => do{
    valis (Tg cut K in createHandler(K,F))
  }

  createHandler:((())=>>result[(),()],()=>result[(),()])=>result[(),()].
  createHandler(K,F) => do{
    (N,Qs) ^= _hdtl(append(append(QQ!,contFromFun(Tg,(_)=>F())),K));
    QQ := Qs;
    logMsg("after spawn $(size(QQ!)) entries");
    N.(())
  }

  join:(string)=>result[(),()].
  join(Nm) => do{
--    logMsg(_stackTrace());
    logMsg("end thread #(Nm) with $(size(QQ!)) entries");
    _ .= (Tg cut K in joinHandler(K));
    valis () -- never coming here
  }

  joinHandler:((())=>>result[(),()]) => result[(),()].
  joinHandler(K) => do{
    schedule()
  }

  schedule:() => result[(),()].
  schedule()=>do{
    logMsg("schedule next");
    if ~isEmpty(QQ!) then{
      logMsg("$(size(QQ!)) entries in Q");
      (N,Qs) ^= _hdtl(QQ!);
      QQ := Qs;
      N.(())
    } else
    logMsg("q was empty");
    valis ()
  }

  contFromFun:all x,y ~~ (tag[x,y],(x)=>y) => ((x)=>>y).
  contFromFun(T,F) => _fun2cont(T,F).

  drainer = tsk(()=>do{
      logMsg("drainer started, $(size(QQ!)) entries");
      while ~isEmpty(QQ!) do{
	logMsg("Q not empty");
	yield("drainer")
      };
      valis ()
    }).

  drainQ()=>do{
    while (N,Qs) ^= _hdtl(QQ!) do {
      QQ := Qs;
      N.(())
    };
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
    Tg prompt {
      logMsg("creating alpha");
      spawnTask(threadMain("alpha"));
      logMsg("creating beta");
      spawnTask(threadMain("beta"));
    };
    logMsg("Q is $(size(QQ!))");
    drainQ()
  }
}
