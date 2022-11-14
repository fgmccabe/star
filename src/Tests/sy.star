test.sy{
  import star.
  import star.script.

  -- Test of syntax of star

  ZZ = case 3 in {
    1 => valof { valis 2}
    2 => valof { valis 0}
    3 => 3
  }

  CX : (cons[integer],integer) => integer.
  CX(Is,Lm) => valof{
    Cx = ref 0;
    
    for Ix in Is do{
      if Ix<Lm then
	Cx:=Cx!+Ix
    };
    
    valis Cx!
  }

  -- another line comment

  public contract all x ~~ lS[x] ::= {
    large : x.
    small : x.
  }

  bind(X) where X>0 => valof{
    logMsg("trying");
    valis .true
  }

  bind(X) where
      X>0 =>
    (MM ?= .some(X) ??
	valof{
	  logMsg("trying");
	  valis .true
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

  o : integer. 
  o = valof{ valis 1}

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

    try{
      assert o == 1;		-- End comment
    } catch {
      _ => {
	logMsg("valof raised exception")
      }
    };

    valis ()
  }
}
