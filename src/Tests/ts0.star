test.ts0{
  -- Simple test of task generator pattern

  comm ::= yield(integer) | .end | .next | .cancel.

  all c ~~ task[c] <~ {}. -- tasks have a type ...

  generator:(integer,integer)=>task[comm].
  generator(F,T) => task{
    Ix .= ref F;
    while Ix! < T do{
      suspend yield(Ix!) in {
	.next => {}.
	cancel => retire .end
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
	yield(X) => do{
	  Tl := Tl! + X
	}
	end => valis Tl!
      }
    }
  }
}
