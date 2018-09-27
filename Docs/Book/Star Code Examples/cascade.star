worksheet {

  primes(Max) is let{
    cascade(F,K) is fn(X)=>(F(X) and X%K!=0)
    prStep((P,F),X) is F(X)?(list of [P..,X],cascade(F,X))||(P,F)
    sieve(C) is first(leftFold(prStep,(list of [],fn K=>K%2!=0),C))
    first((L,R)) is L
  } in sieve(range(3,Max,2))

  show primes(100)
}
