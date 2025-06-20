test.jt11{
  -- Jitting test of fibers

  scomm ::= .yild(integer) | .end.
  rcomm ::= .next | .cancel.

  lt:(integer,integer) => boolean.
  lt(X,Y) => _int_lt(X,Y).

  add:(integer,integer)=>integer.
  add(X,Y) => _int_plus(X,Y).

  genr:(integer,integer)=> fiber[rcomm,scomm].
  genr(F,T) => _fiber(
    (Gen,_) => valof{
      Ix = ref F;
      while lt(Ix!,T) do{
	case Gen suspend .yild(Ix!) in {
	  | .next => {}
	  | .cancel => Gen retire .end
	};
      
	Ix := add(Ix!,1);
      };
      Gen retire .end
    }).

  adder:(integer,integer) => integer.
  adder(F,T) => valof{
    TT = genr(F,T);
    Tl = ref 0;

    while .true do {
      case TT resume .next in {
	| .yild(X) => {
	  logM(conc("add ",conc(_int2str(X),conc(" to ",_int2str(Tl!)))));
	  Tl := add(Tl!,X)
	}
	| .end => valis Tl!
      }
    }
  }

  main:() => ().
  main() => valof{
    logM(_int2str(adder(0,10)));

    try{
      _jit_compile("#(__pkg__)@add",2);
      _jit_compile("#(__pkg__)@adder",2);
      _jit_compile("#(__pkg__)@lt",2);
      _jit_compile("#(__pkg__)@genr",2);
      _jit_compile("#(__pkg__)@logM",1);
      _jit_compile("#(__pkg__)λ_0",3);
    } catch {
      X => logM(_stringOf(X,0))
    };

    logM(_int2str(adder(0,10)));
  }

  conc:(string,string)=>string.
  conc(A,B) => _str_concat(A,B).

  logM:(string)=>().
  logM(M) => valof{
    try{
      _logmsg(M)
    } catch {_ => {}};
    valis ()
  }
  
}
