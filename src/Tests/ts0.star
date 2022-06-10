test.ts0{
  import star.core.
  import star.arith.
  import star.coerce.
  
  -- Simple test of task generator pattern

  comm ::= yield(integer) | .end | .next | .cancel.

  all c ~~ task[c] <~ {}. -- tasks have a type ...

  generator:(integer,integer)=>task[comm].
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

  main:() => ().
  main() => valof{
    _ .= _logmsg(disp(adder(0,10)));
    valis ()
  }
}
