philoshopers is package{
  import concurrency;
    
  semaphore(Count) is {
    private (grabCh, releaseCh) is makeSemaphore(Count)

    grab() is wait for put () on grabCh;
    release() is wait for put () on releaseCh;
  };

  makeSemaphore(Count) is valof {
    ignore background semaphore(Count);
    valis (grabCh, releaseCh)
  } using {
    grabCh is channel();
    releaseCh is channel();
    
    release(x) is choose wrap incoming releaseCh in _ -> semaphore(x+1);
    grab(x) is choose wrap incoming grabCh in _ -> semaphore(x-1);

    semaphore(0) is wait for release(0);
    semaphore(x) default is wait for grab(x) or release(x)
  };
 
  -- Dining philosophers using concurrency primitives
  type sem is alias of  { grab has type ()=>task of (()); release has type ()=>task of (()) };
  
  table(Count) do let{
      T is semaphore(3);
      
      phil has type (integer, sem, sem)=>task of (());
      phil(n,L,R) is task{
        for Ix in iota(1,Count,1) do{
          -- sleep(random(15L));
          perform T.grab();  -- get permission first
          perform L.grab();
          perform R.grab();
          -- logMsg(info,"Phil $n is eating for the $(Ix)th time");
          perform T.release();
          -- logMsg(info,"Table released");
          perform L.release();
          -- logMsg(info,"Left released");
          perform R.release();
          -- logMsg(info,"Right released");
        }
        logMsg(info,"Phil $n ate for $(Count) times");
      };
    } in {
      fork1 is semaphore(1);
      fork2 is semaphore(1);
      fork3 is semaphore(1);
      fork4 is semaphore(1);
    
      phil1 is background phil(1,fork1,fork2);
      phil2 is background phil(2,fork2,fork3);
      phil3 is background phil(3,fork3,fork4);
      phil4 is background phil(4,fork4,fork1);
    
      perform phil1;
      perform phil2;
      perform phil3;
      perform phil4;
    };
    
  main() do {
    table(50000);
  }
}
