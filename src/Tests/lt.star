test.lt{
  import star.
  import star.script.

  genInt:(integer)=>integer.
  genInt(X) => X+10.

  ll(M) => let{.
    S = ref genInt(M).

    f(A) => (N) => (N>=S! ?  A || f(A*N)(N+1)).
  .} in (f(1)(1),S!).

/*  contract all x ~~ zz[x] ::= {
    pl: (x,x)=>x.
    zero: x.
  }.

  implementation zz[integer] => {
    pl(X,Y) => _int_plus(X,Y).
    zero = 0.
  }.
*/
  
  main:()=>action[(),()].
  main()=>action{
    show ll(1);
    show ll(2);
--    show pl(zero,1)
  }
}  
