test.ts2{
  import star.core.
  import star.arith.
  import star.coerce.
  import star.iterable.
  import star.action.

  -- Zero-tuples
  public implementation display[()] => {
    disp(_) => "()".
  }

  public implementation all e ~~ display[e] |: display[cons[e]] => let{.
    consDisp(.nil,L) => L.
    consDisp(cons(X,.nil),L) => cons(disp(X), L).
    consDisp(cons(X,R),L) => cons(disp(X), cons(",", consDisp(R,L))).
 .} in {
    disp(L) => _str_multicat(cons("[",consDisp(L,cons("]",.nil))))
  }
  
  -- Test generator functions

  scomm[e] ::= _yld(e) | ._all.
  rcomm ::= ._next | ._cancel.

  all s,r ~~ task[s,r] <~ {}.		-- tasks have a type ...

  iterTask(L) => generator {
    LL .= ref L;
    while cons(H,T) .= LL! do{
      yield H;
      LL := T
    }
  }
    
  iota:(integer,integer)=>cons[integer].
  iota(F,F) => .nil.
  iota(F,T) => cons(F,iota(F+1,T)).

  odds:(cons[integer]) => ().
  odds(L) => valof{
    try{
      for (X where X%2==1) in L do{
	_ .= _logmsg(disp(X));
	if X>6 then
	  raise ()
      }
    } catch {
      _ => {
	_ .= _logmsg("caught")
      }
    };
    valis ()
  }

  main:() => ().
  main() => valof{
    LL .= iota(1,12);
    _ .= _logmsg(disp(LL));
    valis _logmsg(disp(odds(LL)));
  }
}
