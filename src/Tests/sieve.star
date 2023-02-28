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
	post(this,Ix!,Chnnl)
      }
    } catch { .canceled => {
	_retire(this,.retired_)
    }
    }
  }

  filter:(task[integer],integer,channel[integer],channel[integer]) => () raises exception.
  filter(Tsk,Prm,Chnl,Next) => valof{
    while Nxt .= collect(Tsk,Chnl) do{
      if ~Nxt%Prm == 0 then
	post(Tsk,Nxt,Next)
    };
    valis ()
  }

  sieve:(task[integer],integer,integer,channel[integer]) => integer.
  sieve(this,Cnt,Mx,Chnnl) => valof{
    try{
      Nxt = collect(this,Chnnl);
      if Cnt<Mx then{
	logMsg("Next prime is $(Nxt), $(Cnt) out of $(Mx)");
	NChnl = newChannel();
	try{
	  spawn((T)=>sieve(T,Cnt+1,Mx,NChnl));
	  filter(this,Nxt,Chnnl,NChnl)
	} catch { .canceled => {
	    _retire(this,.retired_) }
	}
      } else{
	logMsg("collected $(Mx) primes");
	_retire(this,.result(Nxt))
      }
    } catch { .canceled => {}};
    _retire(this,.retired_)
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
      _retire(Tsk,.retired_)
    };

    Eras = nursery([Gn,Sv]);
    logMsg("final result $(Eras)");
    valis ()
  }
}

  
