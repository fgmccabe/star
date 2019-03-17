test.sy{
  import star.
  -- Test of syntax of star

  o : action[string,integer]. 
  o = return 1.

  p : action[string,integer].
  p = (o >>= double) >>= double.

  assert _perform(o) == 1.

  assert _perform(p) == 4.

  a = do{
    x <- p;
    return x
  }.

  fact:all e ~~ arith[e] |: (e)=>e. 
  fact(N)=>let{
	     ff(0,F) => F.
	     ff(X,F) where X>0 => ff(X-1,F*X).
	   } in ff(N,1).

  assert valof a == 4.
}
