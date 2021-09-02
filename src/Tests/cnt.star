test.cnt{
  import star.
  import star.script.

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

  createThread(F) =>
    Tg cut K in createHandler(K,F).

  contFromFun:all x,y ~~ (tag[x,y],(x)=>y) => ((x)=>>y).
  contFromFun(T,F) => _fun2cont(T,F).


  createHandler(K,F) => do{
    (N,Qs) ^= _hdtl(append(append(QQ!,contFromFun(Tg,F)),K));
    QQ := Qs;
    N.(())
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
    Tg prompt {    
      _ .= createThread(threadMain("alpha"));
      _ .= createThread(threadMain("beta"));
      valis ()
    }
  }
}    
