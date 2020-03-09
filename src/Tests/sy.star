test.sy{
  import star.
  import star.script.

  -- Test of syntax of star

  CX : (list[integer],integer) => action[(),integer].
  CX(Is,Lm) => do{
    Cx := 0;
    
    for Ix in Is do{
      if Ix<Lm then
	Cx:=Cx!+Ix
    };
    
    return Cx!
  }

  -- another line comment

  public contract all x ~~ lS[x] ::= {
    large : x.
    small : x.
  }

  bind(X) where X>0 => valof action{
    logMsg("trying");
    return .true
  }

  bind(X) where
      X>0 =>
    (MM ^= some(X) ?
	valof action{
	  logMsg("trying");
	  return .true
	}
	|| .false).
  
  foo(X) where
      bind(X) => 1.
  foo(X) where
      bind(X) &&
      foo(X-1) > 1 => 2.
  
  person ::= .noone
    | .someone
    | .everyone.

  o : action[string,integer]. 
  o = return 1.

  p : action[string,integer].
  p = (o >>= double) >>= double.

  double(X) => action{ valis X+X }.
  
  /* Block comment */

  a = do{
    x <- p;
    y <- p;
    return x
  }
  
  fact:all e ~~ equality[e], arith[e],comp[e] |: (e)=>e. 
  fact(N)=>let{
    ff(zero,F) => F.
    ff(X,F) where X>zero => ff(X-one,F*X).
  } in ff(N,one).

  main:() => action[(),()].
  main() => do{
    show "fact(3) = $(fact(3))";
    
    show """a multi
  line
string\n
    """;
 
    assert valof a == 4;

    assert valof o == 1;		-- End comment
  
    assert valof p == 4;
    assert _perform(p) == 4		/* block comment */
  }
}
