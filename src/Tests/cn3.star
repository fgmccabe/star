test.cn3{
  import star.core.
  import test.cn2.
  import star.arith.
  import star.strings.
  import star.display.
  import star.option.
  import star.coerce.

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

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x,e ~~ (s,x,(t,x)=>x) => x
  }
  
  public implementation all e,x ~~ iter[cons[e]->>e] => {
    _iter(.nil,S,_) => S.
    _iter(cons(H,T),S,F) => _iter(T,F(H,S),F).
  }

  public implementation all e ~~ display[e] |: display[cons[e]] => let{
    consDisp(.nil) => ss("").
    consDisp(cons(X,.nil)) => disp(X).
    consDisp(cons(X,R)) => ssSeq([disp(X), ss(","), consDisp(R)]).
  } in {
    disp(L) => ssSeq([ss("["), consDisp(L),ss("]")]).
  }

  public implementation all x,y ~~ display[x], display[y] |: display[(x,y)] =>
    {.
      disp((a,b)) => ssSeq([ss("("),disp(a),ss(" , "),disp(b),ss(")")]).
    .}
}
