test.ta{
  import star.
  import star.script.

  -- Test task expressions

  tsk ::= tsk((())=>result[(),()]).

  Tg = tag().

  QQ : ref qc[(())=>>result[(),()]].
  QQ = ref qc([],[]).

  yield:()=>().
  yield() => Tg cut K in yieldHandler(K).

  yieldHandler:((())=>>result[(),()]) => result[(),()].
  yieldHandler(K) => do{
    (N,Qs) ^= _hdtl(append(QQ!,K));
    QQ := Qs;
    N.(())
  }

  spawnTask(tsk(F)) =>
    Tg cut K in createHandler(K,F).

  createHandler:((())=>>result[(),()],(())=>result[(),()])=>result[(),()].
  createHandler(K,F) => do{
    (N,Qs) ^= _hdtl(append(append(QQ!,contFromFun(Tg,F)),K));
    QQ := Qs;
    N.(())
  }

  contFromFun:all x,y ~~ (tag[x,y],(x)=>y) => ((x)=>>y).
  contFromFun(T,F) => _fun2cont(T,F).

  drainer:tsk.
  drainer = tsk((_)=>do{
    do{
      _.=yield()
    } until isEmpty(QQ!);
    valis ()
    }).

  drainQ:()=>result[(),()].
  drainQ()=>do{
    while (N,Qs) ^= _hdtl(QQ!) do {
      QQ := Qs;
      N.(())
    };
    valis ()
  }

  threadMain:(string)=>tsk.
  threadMain(T) => tsk((_) => do{
    logMsg("A$(T)");
    _.=yield();
    logMsg("B$(T)");
    _ .= yield();
    valis()
    })

  main:()=>action[(),()].
  main() => action{
    Tg prompt {
--      _ .= spawnTask(drainer);
      _ .= spawnTask(threadMain("alpha"));
      _ .= spawnTask(threadMain("beta"));
      drainQ()
--      valis ()
    }
  }
  

}
