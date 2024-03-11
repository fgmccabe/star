test.ms{
  import star.
  import star.assert.
  
  mapOver:all s/1,m/1,a,b ~~ stream[s[a]->>a],sequence[s[b]->>b],monad[m],reversible[s[b]] |:
    ((a)=>m[b],s[a])=>m[s[b]].
  mapOver(F,S)=>let{.
    mpOver([],Sf)=> return reverse(Sf).
    mpOver([E,..Es],Sf) =>
      F(E) >>= (E1)=>mpOver(Es,[E1,..Sf]).
  .} in mpOver(S,[]).


  doubleOrQuits:(integer)=>option[integer].
  doubleOrQuits(X) where X>0 => .some(X+X).
  doubleOrQuits(_) => .none.

  KK : cons[integer].
  KK = [1,2,3,4,5].

  LL : cons[integer].
  LL = [1,2,3,0,5].

  sq:(float)=>option[float].
  sq(X) => (try .some(sqrt(X)) catch errorCode in {_ => .none}).

  sqr:(float)=>either[float,string].
  sqr(X) => (try .either(sqrt(X)) catch errorCode in {_ => .other("negative")}).

  main:() => ().
  main() => valof{
    show mapOver(doubleOrQuits,KK);

    show mapOver(doubleOrQuits,LL);

    assert [2,4,6,8,10] ?= mapOver(doubleOrQuits,KK);

    assert mapOver(doubleOrQuits,LL)==.none;

    show (([1.0,2.0,-3.0]//sq):cons[option[float]]);
    valis ()
  }
}
