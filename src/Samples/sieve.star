sample.sieve{
  import star.
  import star.range.

  primes:(integer) => list[integer].
  primes(Max) => let{
    cascade:((integer)=>boolean,integer) => ((integer)=>boolean).
    cascade(F,K) => (X)=>(F(X) && X%K=!=0).

    step(X,(P,F)) where F(X) => ([P..,X],cascade(F,X)).
    step(_,(P,F)) => (P,F).

    sieve:(range[integer])=>list[integer].
    sieve(R) => fst(foldRight(step,([],(K)=>true),R)).
  } in sieve(range(3,Max,2)).

  show disp(foldRight((+),0,range(3,1001,2)))::string.

  show disp(range(3,1000,2))::string.

  show disp(primes(1001))::string.
}
