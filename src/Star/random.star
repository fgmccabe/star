star.random{
  import star.

  -- Derived from GHC via Mike Sperber

  public StdGen ::= StdGen(integer,integer).

  public contract all g ~~ RandomGen[g] ::= {
    next : (g) => (integer,g).
    split : (g) => (g,g).

    genRange : (g) => (integer,integer)
  }

  public mkStdGen:(integer)=>StdGen.
  mkStdGen(S) where S<0 => mkStdGen(-S).
  mkStdGen(S) => valof action{
    q .= S / 2147483562;
    s1 .= S % 2147483562;
    s2 .= q % 2147483398;
    valis StdGen (s1+1, s2+1)
  }

  public contract all a ~~ Random[a] ::= {
    randomR : all g ~~ RandomGen[g] |: (a,a,g) => (a,g).
    random : all g ~~ RandomGen[g] |: (g) => (a,g).
  }

  randomIVal:all g ~~ RandomGen[g] |: (integer,integer,g) => (integer,g).
  randomIVal(l,h,rng) where l>h => randomIVal(h,l,rng).
  randomIVal(l,h,rng) => valof action{
    k .= h - l + 1;
    b .= 2147483561;
    n .= iLogBase(b, k);

    (v, rng1) .=
      (let {.
	  f(0, acc, g) => (acc, g).
	  f(n1, acc, g) where (x, g1) .= next(g) =>
	    f(n1 - 1, x + acc * b, g1)
.} in
	f(n, 1, rng));
    valis (l + v % k, rng1)
  }

  iLogBase : (integer, integer) => integer.
  iLogBase(b, i) =>
    (i < b) ? 1 || 1 + iLogBase(b, i / b).

  min32Bound = -0x80000000.
  max32Bound = 0x7fffffff.
  int32Range = max32Bound - min32Bound.

  public implementation Random[integer] => {
    randomR(lo, hi, g) => randomIVal(lo, hi, g).
    random(g) => randomIVal(min32Bound, max32Bound, g) -- had to pick something
  }

  public implementation RandomGen[StdGen] => let{.
    stdRange : (StdGen) => (integer, integer).
    stdRange(_) => (0, 2147483562).

  /** eturns values in the range stdRange */
    stdNext : (StdGen) => (integer, StdGen).
    stdNext(StdGen(s1, s2))  => 
      valof action {	
	k .= s1 / 53668;
	s1_1  .= 40014 * (s1 - k * 53668) - k * 12211;
	s1_2 .= (s1_1 < 0 ? s1_1 + 2147483563 || s1_1);

	k1 .= s2 / 52774;
	s2_1  .= 40692 * (s2 - k1 * 52774) - k1 * 3791;
	s2_2 .= (s2_1 < 0 ? s2_1 + 2147483399 || s2_1);

	z  .= s1_2 - s2_2;
	z1 .= (z < 1 ? z + 2147483562 || z);

	valis (z1, StdGen(s1_2, s2_2))
      }.

    stdSplit : (StdGen) => (StdGen, StdGen).
    stdSplit(std) where StdGen(s1, s2) .= std =>
      valof action{
	/* no statistical foundation for this! */
	new_s1 .= (s1 == 2147483562 ? 1 || s1 + 1);
	new_s2 .= (s2 == 1 ? 2147483398 || s2 - 1);
	StdGen(t1, t2) .= snd(stdNext(std));
	left .= StdGen(new_s1, t2);
	right .= StdGen(t1, new_s2);
	valis (left, right)
      }
 .} in {
    next = stdNext.
    split = stdSplit.
    genRange = stdRange
  }
}

    
