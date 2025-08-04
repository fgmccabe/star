test.primes{
  import star.
  import star.assert.
  import star.mbox.

  -- Sieve of Erastosthenes, using collections

  sieve:(integer)=>cons[integer].
  sieve(Bound) => let{.
    prmes([]) => [].
    prmes([P,..Rest]) => [P,..prmes(filter(Rest,P))].
  .} in prmes(iota(2,Bound)).

  filter:(cons[integer],integer)=>cons[integer].
  filter(Candidates,P) => (Candidates ^/ nonMultiple(P)).

  nonMultiple(P) => (X)=>~divides(P,X).

  -- Sieve of erastosthenes using fold

  primes:(integer) => cons[integer].
  primes(Max) => let{.
    cascade:((integer)=>boolean,integer) => ((integer)=>boolean).
    cascade(F,K) => (X)=>(F(X) && ~divides(K,X)).

    step:(integer,(cons[integer],(integer)=>boolean)) => (cons[integer],(integer)=>boolean).
    step(X,(P,F)) where F(X) => ([X,..P],cascade(F,X)).
    step(_,(P,F)) => (P,F).

    sieve:(range[integer])=>cons[integer].
    sieve(R) => fst(foldRight(step,([],(K)=>.true),R)).
  .} in sieve(3..<Max:2).
  
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
    show "$(Cnt) sieve = $(sieve(Cnt))";
    show "$(Cnt) primes = $(primes(Cnt))";
    show "$(Cnt) prime sieve = $(primeSieve(Cnt,genOdds))"
  }
}

  
