test.ts0{
  import star.core.
  import star.arith.
  import star.coerce.
  
  -- Simple test of generator pattern

  scomm ::= .yild(integer) | .end | .identify(rcomm=>>scomm).
  rcomm ::= .next | .cancel.

  genr:(integer,integer)=> (rcomm=>>scomm).
  genr(F,T) => case (Gen spawn valof{
    Gen suspend .identify(Gen);
    Ix = ref F;
    while Ix! < T do{
      case (Gen suspend .yild(Ix!)) in {
	.next => {}.
	.cancel => Gen retire.end
      };
      
      Ix := Ix! + 1;
    };
    Gen retire .end
    }) in {
    .identify(G) => G
    }

  adder:(integer,integer) => integer.
  adder(F,T) => valof{
    TT = genr(F,T);
    Tl = ref 0;

    while .true do {
      case (TT resume .next) in {
	.yild(X) => {
--	  _logmsg("add $(X) to $(Tl!)");
	  Tl := Tl! + X
	}.
	.end => valis Tl!
      }
    }
  }

  main:() => ().
  main() => valof{
    valis logM(disp(adder(0,10)));
  }

  logM:(string)=>().
  logM(M) => valof{
    try{
      _logmsg(M)
    } catch errorCode in {_ => {}};
    valis ()
  }
  
}
