test.ms{
  import star.
  import star.script.
  
  mapOver:all s/1,m/1,a,b ~~ stream[s[a]->>a],sequence[s[b]->>b],monad[m] |:
    ((a)=>m[b],s[a])=>m[s[b]].
  mapOver(F,S)=>let{
    mpOver([],Sf)=> return Sf.
    mpOver([E,..Es],Sf) =>
      F(E) >>= (E1)=>mpOver(Es,[Sf..,E1]).
  } in mpOver(S,[]).


  doubleOrQuits:(integer)=>option[integer].
  doubleOrQuits(X) where X>0 => some(X+X).
  doubleOrQuits(_) => .none.

  KK : list[integer].
  KK = [1,2,3,4,5].

  LL : list[integer].
  LL = [1,2,3,0,5].

  sq:(float)=>option[float].
  sq(X) where X>0.0 => some(sqrt(X)).
  sq(_) default => .none.

  sqr:(float)=>either[string,float].
  sqr(X) where X>0.0 => either(sqrt(X)).
  sqr(_) default => other("negative").
  

  main:() => action[(),()].
  main() => do{
    show "$(mapOver(doubleOrQuits,KK))";

    show "$(mapOver(doubleOrQuits,LL))";

    assert [2,4,6,8,10] ^= mapOver(doubleOrQuits,KK);

    assert mapOver(doubleOrQuits,LL)==.none;

    show disp(strmap(sq,([1.0,2.0,3.0,4.0]:list[float])));

    show disp(strmap(sq,([1.0,2.0,-3.0]:cons[float])));

    show disp(seqmap(sqr,([1.0,2.0,3.0,4.0]:list[float])));
    show disp(seqmap(sqr,([1.0,-2.0,3.0,4.0]:list[float])))
  }
}
