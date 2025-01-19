test.cns{
  import star.core.
  import star.monad.
  import test.ar.

  -- stream & sequence contracts
  public implementation all x ~~ stream[cons[x] ->> x] => {
    _eof(.nil) => .true.
    _eof(cons(_,_)) => .false.
    
    _hdtl(cons(H,T)) => some((H,T)).
    _hdtl(.nil) => .none.
  }

  public implementation all x ~~ sequence[cons[x] ->> x] => {
    _cons(E,S) => cons(E,S).
    _nil = .nil.
  }

  public implementation all e ~~ display[e] |: display[cons[e]] => let{.
    consDisp(.nil) => ss("").
    consDisp(cons(X,.nil)) => disp(X).
    consDisp(cons(X,R)) => ssSeq([disp(X), ss(","), consDisp(R)]).
  } in {
    disp(L) => ssSeq([ss("["), consDisp(L),ss("]")]).
  }

  public len:all a~~(cons[a])=>integer.
  len(.nil)=>0.
  len(cons(_,L))=>len(L)+1.

  public nth:all a~~(cons[a],integer)=>a.
  nth(cons(E,_),0) => E.
  nth(cons(_,L),Ix) => nth(L,Ix-1).

  public implementation functor[cons] => let{.
    fm:all a,b ~~ ((a)=>b,cons[a])=>cons[b].
    fm(_,.nil) => .nil.
    fm(f,cons(H,T)) => cons(f(H),fm(f,T))
  } in {
    fmap = fm.
  }
}
