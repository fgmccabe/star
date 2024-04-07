test.eras{
  import star.
  import star.assert.
  import star.mbox.

  -- Sieve of Erastosthnese, using futures.

  intGen = generator{
    Ix := 1;
    while .true do{
      Ix := Ix!+2;
      yield Ix!
    }
  }

  divides(X,Y) => (try X%Y==0 catch exception in {_ => .false}).

  filter:(integer,generater[integer]) => generater[integer].
  filter(Prm,Gen) => generator{
    for Nxt : Gen do{
      if ~divides(Nxt,Prm) then
	yield(Nxt)
    }
  }

  sieve:(integer,integer,integer,generater[integer]) => integer.
  sieve(Cnt,Mx,Pr,Gen) => valof{
    case _resume(Gen,._next) in {
      ._yld(Nxt) => {
	if Cnt<Mx then{
	  logMsg("Next prime is $(Nxt), $(Cnt) out of $(Mx)");
	  valis sieve(Cnt+1,Mx,Nxt,filter(Nxt,Gen))
	}
      }
    };
    valis Pr
  }

  _main:(cons[string]) => ().
  _main([C,.._]) where Cnt?=(C:?integer) => main(Cnt).
  _main(_) => main(100).

  main:(integer)=>().
  main(Cnt) => valof{
    MxPrime = sieve(0,Cnt,1,intGen);
    logMsg("final result $(MxPrime)");
    valis ()
  }
}

  
