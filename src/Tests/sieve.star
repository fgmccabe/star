test.sieve{
  import star.
  import star.script.
  import star.mbox.

  gen:(channel[integer]) => taskFun[integer].
  gen(Chnnl) => (this)=>valof{
    Ix := 1;
    try{
      while .true do{
	Ix := Ix!+2;
	post(Ix!,Chnnl)
      }
    } catch { .canceled => {
	this retire .retired_
    }
    }
  }

  filter:(this : task[integer]), raises mboxException |: (integer,channel[integer],channel[integer]) => ().
  filter(Prm,Chnl,Next) => valof{
    while Nxt .= collect(Chnl) do{
      if ~Nxt%Prm == 0 then
	post(Nxt,Next)
    };
    valis ()
  }

  sieve:(task[integer],integer,integer,channel[integer]) => ().
  sieve(this,Cnt,Mx,Chnnl) => valof{
    try{
      Nxt = collect(Chnnl);
      if Cnt<Mx then{
	logMsg("Next prime is $(Nxt), $(Cnt) out of $(Mx)");
	NChnl = newChannel();
	try{
	  this suspend .fork((T)=>sieve(T,Cnt+1,Mx,NChnl));
	  filter(Nxt,Chnnl,NChnl)
	} catch { .canceled => {
	    this retire .retired_ }
	}
      } else{
	logMsg("collected $(Mx) primes");
	this retire .result(Nxt)
      }
    } catch { .canceled => {}};
    this retire .retired_
  }

  _main:(cons[string]) => ().
  _main([C,.._]) where Cnt?=(C:?integer) => main(Cnt).
  _main(_) => main(100).

  main:(integer)=>().
  main(Cnt) => valof{
    FstCh = newChannel();
    Gn = gen(FstCh);
    Sv = (Tsk)=>valof{
      sieve(Tsk,0,Cnt,FstCh);
      Tsk retire .retired_
    };

    try{
      Eras = nursery([Gn,Sv]);
      logMsg("final result $(Eras)");
    } catch {
      .deadlock => logMsg("Sieve got deadlocked")
    };
    valis ()
  }
}

  
