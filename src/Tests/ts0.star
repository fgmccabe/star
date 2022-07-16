test.ts0{
  import star.core.
  import star.arith.
  import star.coerce.
  
  -- Simple test of task generator pattern

  scomm ::= yild(integer) | .end.
  rcomm ::= .next | .cancel.

  all s,r ~~ task[s,r] <~ {}. -- tasks have a type ...

  generatr:(integer,integer)=>task[rcomm,scomm].
  generatr(F,T) => task{
    Ix = ref F;
    while Ix! < T do{
      suspend yild(Ix!) in {
	.next => {}.
	.cancel => retire .end
      };
      
      Ix := Ix! + 1;
    };
    valis .end
  }

  adder:(integer,integer) => integer.
  adder(F,T) => valof{
    TT = generatr(F,T);
    Tl = ref 0;

    while .true do {
      TT resume .next in {
	yild(X) => {
	  Tl := Tl! + X
	}.
	.end => valis Tl!
      }
    }
  }

  main:() => ().
  main() => valof{
    valis _logmsg(disp(adder(0,10)));
  }
}
