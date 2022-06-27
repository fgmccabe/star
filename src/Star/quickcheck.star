star.quickcheck{
  import star.
  import star.random.

  gen[a] ::= Gen((integer,StdGen)=>a).

  sized:all a ~~ ((integer)=>gen[a])=>gen[a].
  sized(f) => Gen(
    (n,r)=> valof{
      Gen(m) .= f(n);
      valis m(n,r)
    }).

  resize:all a ~~ (integer,gen[a])=>gen[a].
  resize(n,Gen(m)) => Gen((_,r)=>m(n,r)).

  rand:gen[StdGen].
  rand = Gen((n,r) => r).

  promote:all a,b ~~ ((a)=>gen[b])=>gen[(a)=>b].
  promote(f) => Gen((n,r) =>
      (a)=>valof {
	Gen(m).=f(a);
	valis m(n,r)
      }).

  variant:all a ~~ (integer,gen[a])=>gen[a].
  variant(V,Gen(m)) =>
    Gen((n,r) =>
	m(n,repeat(((R)=>snd(split(r))),V+1)(r))).

  generate:all a ~~ (integer,StdGen,gen[a])=>a.
  generate(n,rnd,Gen(m)) where (Size,Rnd1).=randomR(0,n,rnd) => m(Size,Rnd1).

  genReturn:all a ~~ (a)=>gen[a].
  genReturn(A) => Gen((n,r)=>A).

  genBind:all a,b ~~ (gen[a],(a)=>gen[b])=>gen[b].
  genBind(Gen(m),k) =>
    Gen((n,r0) where (r1,r2) .= split(r0) && Gen(m1).=k(m(n,r1)) => m1(n,r2)).

  genLiftM2:all a,b,c ~~ ((a,b)=>c) => (gen[a],gen[b])=>gen[c].
  genLiftM2(f) =>
    (ga,gb)=>
      genBind(ga,(a)=>
	  genBind(gb,
	    (b)=>genReturn(f(a,b)))).
  
  genLiftM3:all a,b,c,d ~~ ((a,b,c)=>d) => (gen[a],gen[b],gen[c])=>gen[d].
  genLiftM3(f) =>
    (ga,gb, gc)=>
      genBind(ga,(a)=>
	  genBind(gb,
	    (b)=>
	      genBind(gc,
		(c)=>genReturn(f(a,b,c))))).

  genFMap:all a,b ~~ ((a)=>b,gen[a]) => gen[b].
  genFMap(f,m) => genBind(m,(x)=>genReturn(f(x))).

  public contract all a ~~ arbitrary[a] ::= {
    arb:gen[a].
    coarb:all b ~~ (a,gen[b])=>gen[b]
  }
    
  
}
  
