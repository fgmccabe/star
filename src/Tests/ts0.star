test.ts0{
  import star.core.
  import star.arith.
  import star.coerce.
  
  -- Simple test of generator pattern

  scomm ::= .yild(integer) | .end.
  rcomm ::= .next | .cancel.

  genr:(integer,integer)=> fiber[rcomm,scomm].
  genr(F,T) => _fiber(
    (Gen,_) => valof{
      Ix = ref F;
      while Ix! < T do{
	case _suspend(Gen,.yild(Ix!)) in {
	  | .next => {}
	  | .cancel => _retire(Gen,.end)
	};
      
	Ix := Ix! + 1;
      };
      _retire(Gen,.end)
    }).

  adder:(integer,integer) => integer.
  adder(F,T) => valof{
    TT = genr(F,T);
    Tl = ref 0;

    while .true do {
      case _resume(TT,.next) in {
	| .yild(X) => {
--	  _logmsg("add $(X) to $(Tl!)");
	  Tl := Tl! + X
	}
	| .end => valis Tl!
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
