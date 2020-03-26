star.range{
  import star.core.
  import star.arith.
  import star.collection.
  import star.cons.

  public all a ~~ range[a]::=range(a,a,a).

  public implementation all a ~~ arith[a],comp[a] |: stream[range[a]->>a] => {.
    _eof(range(F,T,_)) => F>=T.
    _hdtl(range(F,T,S)) where F<T => some((F,range(F+S,T,S))).
    _hdtl(_) default => .none.
  .}

  public implementation all a ~~ arith[a],comp[a] |:
    sequence[range[a]->>a] => {.
    _cons(F,range(_,T,S)) => range(F,T,S).
    _nil = range(zero,zero,one).
  .}

  public implementation all a~~arith[a],comp[a] |: folding[range[a]->>a] => {
    foldLeft(F,X,range(Fr,To,St)) => rangeLeft(F,X,Fr,To,St).
    foldRight(F,X,range(Fr,To,St)) => rangeRight(F,X,Fr,To,St).

    rangeLeft:all x ~~ (((x,a)=>x),x,a,a,a) => x.
    rangeLeft(F,Z,Fr,To,_) where Fr>=To => Z.
    rangeLeft(F,Z,Fr,To,St) => rangeLeft(F,F(Z,To),Fr,To-St,St).

    rangeRight:all x ~~ (((a,x)=>x),x,a,a,a) => x.
    rangeRight(_,Z,Fr,To,_) where Fr>=To => Z.
    rangeRight(F,Z,Fr,To,St) => rangeRight(F,F(Fr,Z),Fr+St,To,St).
  }

  public implementation all a ~~ display[a] |: display[range[a]] => {.
    disp(range(F,T,St)) => ssSeq([disp(F),ss(":"),disp(T),ss("@"),disp(St)]).
  .}
}
