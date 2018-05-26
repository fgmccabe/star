star.either{
  import star.core.
  import star.arith.
  import star.lists.

  -- Either or values
  public all a,b ~~ either[a,b] ::= either(a) | other(b).

  -- Display either-or values
  public implementation all x,y ~~ display[x], display[y] |: display[either[x,y]] => {
    disp(T) => dispEither(T).
  }

  private dispEither:all x,y ~~ display[x], display[y] |: (either[x,y]) => ss.
  dispEither(either(a)) => ssSeq([ss("either("),disp(a),ss(")")]).
  dispEither(other(b)) => ssSeq([ss("or("),disp(b),ss(")")]).

  public implementation all x,y ~~ equality[x], equality[y] |: equality[either[x,y]] => {.
    eitherEquals:all x,y ~~ equality[x], equality[y] |: (either[x,y],either[x,y])=>boolean.
    eitherEquals(either(A),either(B)) => A==B.
    eitherEquals(other(A),other(B)) => A==B.
    eitherEquals(_,_) => false.

    X == Y => eitherEquals(X,Y).
  .}

  public implementation all x,y ~~ hash[x],hash[y] |: hash[either[x,y]] => {.
    eitherHash:all x,y ~~ hash[x],hash[y] |: (either[x,y])=>integer.
    eitherHash(either(A)) => hash(A)*37.
    eitherHash(other(B)) => hash(B)*41.

    hash(X) => eitherHash(X).
  .}
}
