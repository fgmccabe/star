test.bench.sieve{
  import star.
  import test.lib.timer.
  import star.assert.

  -- Sieve of Erastosthenes, using collections

  sieve:(integer)=>cons[integer].
  sieve(Bound) => let{.
    prmes([]) => [].
    prmes([P,..Rest]) => [P,..prmes(filter(Rest,P))].
  .} in prmes(iota(2,Bound)).

  filter:(cons[integer],integer)=>cons[integer].
  filter(Candidates,P) => (Candidates ^/ nonMultiple(P)).

  nonMultiple(P) => (X)=>~divides(P,X).

  divides(X,Y) => (try Y%X==0 catch {_ => .false}).

  test(count) => 
    verfy([|sieve(count)|],count).

  verfy(669,5000) => .true.
  verfy(_,_) default => .false.

  public sieveBenchTest()=>timeOf((){ test(5000) }).

  main:(integer){}.
  main(Cnt){
    timer = timer_start((1.<<.Cnt)-1, "Sieve benchmark");

    assert test(Cnt);

    timer_finish(timer);
  }
}
  
  
