test.eras{
  import star.
  import star.assert.
  import star.mbox.

  -- Sieve of Erastosthnese, using generators

  intGen = generator{
    Ix := 1;
    while .true do{
      Ix := Ix!+2;
      yield Ix!
    }
  }

  divides(X,Y) => (try X%Y==0 catch {_ => .false}).

  filter:(integer,generator[integer]) => generator[integer].
  filter(Prm,Gen) => generator{
    for Nxt : Gen do{
      if ~divides(Nxt,Prm) then
	yield(Nxt)
    }
  }

  sieve:(integer,integer,integer,generator[integer]) => integer.
  sieve(Cnt,Mx,Pr,Gen) => valof{
    try{
      Nxt = Gen resume ._next;
      if Cnt<Mx then{
--	showMsg("Next prime is $(Nxt), $(Cnt) out of $(Mx)");
	valis sieve(Cnt+1,Mx,Nxt,filter(Nxt,Gen))
      }
    } catch {
      | ._all do {}
    };
    valis Pr
  }

  _main:(cons[string]) => integer.
  _main([C,.._]) where Cnt.=(C:?integer) => valof{ main(Cnt); valis 0}.
  _main(_) => valof { main(100); valis 0}.

  main:(integer){}.
  main(Cnt){
    MxPrime = sieve(0,Cnt,1,intGen);
    showMsg("final result $(MxPrime)");
  }
}

  
