test.sieve{
  import star.
  import star.assert.
  import star.mbox.

  gen:(receiver[integer]) => taskFun[integer].
  gen(Chnnl) => (this)=>valof{
    Ix := 1;
    try{
      while .true do{
	Ix := Ix!+2;
	post(Ix!,Chnnl)
      }
    } catch mboxException in { .canceled => {
	this retire .retired_
    }
    }
  }

  divides(X,Y) => (try X%Y==0 catch exception in {_ => .false}).

  filter:(this : task[integer]), raises mboxException |: (integer,emitter[integer],receiver[integer]) => ().
  filter(Prm,Chnl,Next) => valof{
    while Nxt .= collect(Chnl) do{
      if ~divides(Nxt,Prm) then
	post(Nxt,Next)
    };
    valis ()
  }

  sieve:(task[integer],integer,integer,emitter[integer]) => integer.
  sieve(this,Cnt,Mx,Chnnl) => valof{
    try{
      Nxt = collect(Chnnl);
      if Cnt<Mx then{
	logMsg("Next prime is $(Nxt), $(Cnt) out of $(Mx)");
	(PChnl,NChnl) = newSlot();
	this suspend .fork((T)=>sieve(T,Cnt+1,Mx,PChnl));
	filter(Nxt,Chnnl,NChnl)
      } else{
	logMsg("collected $(Mx) primes");
	this retire .result(Nxt)
      }
    } catch mboxException in { .canceled => {}};
    this retire .retired_
  }

  _main:(cons[string]) => ().
  _main([C,.._]) where Cnt?=(C:?integer) => main(Cnt).
  _main(_) => main(100).

  main:(integer)=>().
  main(Cnt) => valof{
    (FstCh,IChnnl) = newSlot();
    Gn = gen(IChnnl);
    Sv = (Tsk)=>valof{
      sieve(Tsk,0,Cnt,FstCh);
      Tsk retire .retired_
    };

    try{
      Eras = nursery([Gn,Sv]);
      logMsg("final result $(Eras)");
    } catch mboxException in {
      .deadlock => logMsg("Sieve got deadlocked")
    };
    valis ()
  }
}

  
