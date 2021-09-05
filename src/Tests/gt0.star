test.gt0{
  import star.
  import star.script.

  -- More experiments in green threads

  Tg = tag().

  QQ : ref qc[(())=>>result[(),()]].
  QQ = ref qc([],[]).

  yield:()=>().
  yield() => Tg cut K in yieldHandler(K).

  yieldHandler:((())=>>result[(),()]) => result[(),()].
  yieldHandler(K) => do{
    logMsg("yielding, $(size(QQ!)) in queue");
    (N,Qs) ^= _hdtl(append(QQ!,K));
    QQ := Qs;
    logMsg("entering next");
    N.(())
  }

  join:()=>result[(),()].
  join()=>do{
    if (N,Qs) ^= _hdtl(QQ!) then{
      QQ := Qs;
      N.(())
    };
    valis ()
  }

  eventLoop() => do{
    while ~isEmpty(QQ!) do{
      logMsg("event loop");
      Tg prompt {
	(N,Qs) ^= _hdtl(QQ!);
	QQ := Qs;
	logMsg("entering next");
	N.(());
      }
    };
    valis ()
  }

  createThread(F) => do{
    QQ := 
      append(QQ!,
	contFromFun(Tg,(_)=>do{
	    logMsg("start");
	    F(());
	    logMsg("end");
	  }));
    valis ()
  }
  
  contFromFun:all x,y ~~ (tag[x,y],(x)=>y) => ((x)=>>y).
  contFromFun(T,F) => _fun2cont(T,F).

  threadMain:(string)=>(())=>result[(),()].
  threadMain(T) => (_) => do{
    logMsg("A$(T)");
    _.=yield();
    logMsg("B$(T)")
  }

  main:()=>action[(),()].
  main() => action{
    _ .= createThread(threadMain("alpha"));
    _ .= createThread(threadMain("beta"));
    _ .= createThread(threadMain("gamma"));

    eventLoop();
  }
}  
