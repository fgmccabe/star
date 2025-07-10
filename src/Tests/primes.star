test.primes{
  import star.
  import star.assert.
  import star.mbox.

  -- Sieve of Erastosthenes, using collections

  sieve:(integer)=>cons[integer].
  sieve(Bound) => let{.
    primes([]) => [].
    primes([P,..Rest]) => [P,..primes(filter(Rest,P))].
  .} in primes(iota(2,Bound)).

  filter:(cons[integer],integer)=>cons[integer].
  filter(Candidates,P) => (Candidates ^/ nonMultiple(P)).

  nonMultiple(P) => (X)=>~divides(P,X).
  
  -- Sieve of Erastosthenes, using generators

  genOdds = generator{
    Ix := 1;
    while .true do{
      Ix := Ix!+2;
      yield Ix!
    }
  }
  
  primeFilter:(integer,generator[integer])=>generator[integer].
  primeFilter(Prm,Gen) => generator{
    for Nxt : Gen do{
      if ~divides(Prm,Nxt) then
	yield Nxt
    }
  }

  primeSieve:(integer,generator[integer]) => integer.
  primeSieve(Limit,Gen) => valof{
    case Gen resume ._next in {
      | ._yld(Nxt) => {
	if Nxt < Limit then
	  valis primeSieve(Limit,primeFilter(Nxt,Gen))
	else
	valis Nxt
      }
    }
  }.

  divides(X,Y) => (try Y%X==0 catch {_ => .false}).

  _main:(cons[string]) => ().
  _main([C,.._]) where Cnt?=(C:?integer) => main(Cnt).
  _main(_) => main(100).

  main:(integer)=>().
  main(Cnt) => valof{
    show sieve(Cnt);
    show primeSieve(Cnt,genOdds)
  }
}

  
