star.tuples{
  import star.core.
  import star.arith.
  import star.lists.

  -- Some basic stuff for tuples
  public implementation display[()] => {
    disp(_) => ss("()").
  }

  public implementation equality[()] => {
    () == () => true.
  }

  public implementation hash[()] => {
    hash(()) => 0.
  }

  -- 2-tuples
  public implementation all x,y ~~ display[x], display[y] |: display[(x,y)] => {.
    disp((a,b)) => ssSeq([ss("("),disp(a),ss(" , "),disp(b),ss(")")]).
  .}

  public implementation all x,y ~~ equality[x], equality[y] |: equality[(x,y)] => {.
    (A1,A2)==(B1,B2) => A1==B1 && A2==B2.
  .}

  public implementation all x,y ~~ hash[x], hash[y] |: hash[(x,y)] => {.
    hash((A,B)) => hash(A)*37+hash(B).
  .}
}
