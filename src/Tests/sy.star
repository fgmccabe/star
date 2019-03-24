test.sy{
  import star.
  -- Test of syntax of star

  o : action[string,integer]. 
  o = return 1.

  p : action[string,integer].
  p = (o >>= double) >>= double.

  assert _perform(o) == 1.		-- End comment

  /* Block comment */
  assert _perform(p) == 4.		/* block comment */

  a = do{
    x <- p; 
    return x
    }

  fact:all e ~~ arith[e] |: (e)=>e. 
  fact(N)=>let{
    ff(0,F) => F.
    ff(X,F) where X>0 => ff(X-1,F*X).
    } in ff(N,1).

  assert valof a == 4.

  CX : (list[integer],integer) => action[(),integer].
  CX(Is,Lm) => do{
    Cx := 0;

    for Ix in Is do{
      if Ix<Lm then
	Cx:=Cx!+Ix
      };

    return Cx!
    }

}
