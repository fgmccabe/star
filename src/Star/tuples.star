star.tuples{
  import star.core.
  import star.arith.
  import star.lists.
  import star.coerce.

  -- Some basic stuff for tuples
  -- Zero-tuples
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

  public fst:all x,y ~~ ((x,y)) => x.
  fst((X,_)) => X.

  public snd:all x,y ~~ ((x,y)) => y.
  snd((_,Y)) => Y.


  -- 3-tuples
  public implementation all x,y,z ~~ display[x], display[y], display[z] |: display[(x,y,z)] => {.
    disp((a,b,c)) => ssSeq([ss("("),disp(a),ss(" , "),disp(b),ss(" , "),disp(c),ss(")")]).
  .}

  public implementation all x,y,z ~~ equality[x], equality[y],equality[z] |: equality[(x,y,z)] => {.
    (A1,A2,A3)==(B1,B2,B3) => A1==B1 && A2==B2 && A3==B3.
  .}

  public implementation all x,y,z ~~ hash[x], hash[y],hash[z] |: hash[(x,y,z)] => {.
    hash((A,B,C)) => (hash(A)*37+hash(B))*37+hash(C).
  .}
}
