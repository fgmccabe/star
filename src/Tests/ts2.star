test.ts2{
  import star.
  import star.script.

  -- Zero-tuples
  public implementation display[()] => {
    disp(_) => "()".
  }

  public implementation all e ~~ display[e] |: display[cons[e]] => let{.
    consDisp(.nil,L) => L.
    consDisp(.cons(X,.nil),L) => .cons(disp(X), L).
    consDisp(.cons(X,R),L) => .cons(disp(X), .cons(",", consDisp(R,L))).
 .} in {
    disp(L) => _str_multicat(.cons("[",consDisp(L,.cons("]",.nil))))
  }
  
  -- Test generator functions

  iterTask(L) => generator {
    LL = ref L;
    while .cons(H,T) .= LL! do{
      yield H;
      LL := T
    }
  }

  isEven(X) => X.&.1==0.
    
  iota:(integer,integer)=>cons[integer].
  iota(F,F) => .nil.
  iota(F,T) => .cons(F,iota(F+1,T)).

  odds:(cons[integer]) => ().
  odds(L) => valof{
    try{
      for (X where ~isEven(X)) in L do{
	logMsg(disp(X));
	if X>6 then
	  raise ()
      }
    } catch () in {
      _ => {
	logMsg("caught")
      }
    };
    valis ()
  }

  main:() => ().
  main() => valof{
    LL = iota(1,12);
    logMsg(disp(LL));
    valis logMsg(disp(odds(LL)));
  }
}
