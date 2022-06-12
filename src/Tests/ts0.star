test.ts0{
  import star.core.
  import star.arith.
  import star.coerce.
  
  -- Simple test of task generator pattern

  scomm ::= yield(integer) | .end.
  rcomm ::= .next | .cancel.

  all s,r ~~ task[s,r] <~ {}. -- tasks have a type ...

  generator:(integer,integer)=>task[scomm,rcomm].
  generator(F,T) => task{
    Ix .= ref F;
    while Ix! < T do{
      suspend yield(Ix!) in {
	.next => {}.
	.cancel => retire .end
      };
      
      Ix := Ix! + 1;
    };
    retire .end
  }

  adder:(integer,integer) => integer.
  adder(F,T) => valof{
    TT .= generator(F,T);
    Tl .= ref 0;

    while .true do {
      TT resume .next in {
	yield(X) => {
	  Tl := Tl! + X
	}.
	.end => valis Tl!
      }
    }
  }

  public result[e,a] ::= ok(a) | bad(e).

  bb:()=>result[string,integer].
  bb() => 

  aa() => valof{
    try{
      DD .= case bb() in {
	ok(X) => X.
	bad(E) => raise E
      }
    } catch (E) in {
      .error => valis ()
    }

  main:() => ().
  main() => valof{
    valis _logmsg(disp(adder(0,10)));
  }
}
