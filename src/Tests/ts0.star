test.ts0{
  import star.core.
  import star.arith.
  import star.coerce.
  import star.fiber.
  
  -- Simple test of fiber generator pattern

  scomm ::= .yild(integer) | .end.
  rcomm ::= .next | .cancel.

  generatr:(integer,integer)=>fiber[rcomm,scomm].
  generatr(F,T) => fiber{
    Ix = ref F;
    while Ix! < T do{
      case _suspend(this,.yild(Ix!)) in {
	.next => {}.
	.cancel => _retire(this,.end)
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
      case _resume(TT,.next) in {
	.yild(X) => {
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
