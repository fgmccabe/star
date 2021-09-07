test.c2{
  import star.
  import star.script.

  -- Test conditional expressions

  private
  cP:all a,b ~~ (option[a],option[b])=>option[(a,b)].
  cP(some(A),some(B)) => some((A,B)).
  cP(_,_) default => .none.

  testP:all x~~comp[x]|:(x,x)=>string.
  testP(A,B) => A<B ? "fred" || "bill".

  main:()=>action[(),()].
  main()=>action{
    assert cP(some(1),some(2)) == some((1,2));
    assert testP("a","b")=="fred"
  }
}  
  
