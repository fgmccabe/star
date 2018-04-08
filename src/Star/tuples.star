star.tuples{
  import star.core.
  import star.arith.

  -- Some basic stuff for tuples
  public implementation display[()] => {
    disp(_) => ss("()").
  }

  public implementation equality[()] => {
    () == () => true.
    hash(()) => 0.
  }

  -- Display 2-tuples
  public implementation all x,y ~~ display[x], display[y] |: display[(x,y)] => {
    disp(T) => dispPair(T).
  }

  private dispPair:all x,y ~~ display[x], display[y] |: ((x,y)) => ss.
  dispPair((a,b)) => ssSeq([ss("("),disp(a),ss(" , "),disp(b),ss(")")]).

  public implementation all x,y ~~ equality[x], equality[y] |: equality[(x,y)] => {
    X == Y => pairEquals(X,Y).
    hash(X) => pairHash(X).
  }

  pairEquals:all x,y ~~ equality[x], equality[y] |: ((x,y),(x,y))=>boolean.
  pairEquals((A1,A2),(B1,B2)) => A1==B1 && A2==B2.

  pairHash:all x,y ~~ equality[x],equality[y] |: ((x,y))=>integer.
  pairHash((A,B)) => hash(A)*37+hash(B).
}
