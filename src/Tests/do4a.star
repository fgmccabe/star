test.do4a{
  import star.
  import star.range.

  import star.script.

    -- Test action notation (for loop)

  doFact:(integer) => action[(),integer].
  doFact(X) =>
    ((Fx) => _sequence(_iter(range(1,X + 1,1),_valis(()),
	  let{.
	    sF(Ix,St) =>
	      _sequence((Fx := Fx !! * Ix),
		(_) => _valis(())).
	    sF(_,St) => _valis(St)
	  .} in sF),
	(_) => _valis(Fx !!)))(ref 1).

  main:()=>action[(),()].
  main() => do{
    show valof doFact(4);
    assert valof doFact(4) == 24
  }
}
  
