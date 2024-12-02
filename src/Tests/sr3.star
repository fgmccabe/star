test.sr3{
  import star.
  import star.assert.

  -- Potential translation of effect handlers

  /* try
  5+ throw .inc(3)
  handle proto[integer] in {
    | .inc(X) => continue X+1
  }
  */

  all x ~~ proto[x] ::= .inc(x).
  
  hendler:(integer)=>integer.
  hendler(L) =>
    (reset(
      let{
	hndl(.inc(X)) => (shift K in K.(X+1))
      } in L+hndl(.inc(3)))).

  main:()=>().
  main() => valof{
    show hendler(5);
    assert hendler(4)==8;
  }
}
