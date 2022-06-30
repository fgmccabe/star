test.sy{
  import star.
  import star.script.

  -- Test of syntax of star

  CX : (cons[integer],integer) => result[(),integer].
  CX(Is,Lm) => do{
    Cx .= ref 0;
    
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

  bind(X) where X>0 => valof{
    logMsg("trying");
    return .true
  }

  bind(X) where
      X>0 =>
    (MM ^= some(X) ?
	valof{
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

  o : result[string,integer]. 
  o = do{ valis 1}

  /* Block comment */

  fact:all e ~~ equality[e], arith[e],comp[e] |: (e)=>e. 
  fact(N)=>let{.
    ff(zero,F) => F.
    ff(X,F) where X>zero => ff(X-one,F*X).
  .} in ff(N,one).

  main:() => ().
  main() => valof{
    show "fact(3) = $(fact(3))";
    
    show """a multi
  line
string\n
    """;
 
    assert valof o == 1;		-- End comment

    valis ()
  }
}
