star.either{
  import star.core.
  import star.arith.
  import star.lists.

  -- Either or values
  public all a,b ~~ either[a,b] ::= either(a) | other(b).

  -- Display either-or values
  public implementation all x,y ~~ display[x], display[y] |: display[either[x,y]] => {.
    disp(either(a)) => ssSeq([ss("either("),disp(a),ss(")")]).
    disp(other(b)) => ssSeq([ss("or("),disp(b),ss(")")]).
  .}

  public implementation all x,y ~~ equality[x], equality[y] |: equality[either[x,y]] => {.
    either(A)==either(B) => A==B.
    other(A)==other(B) => A==B.
    _==_ => false.
  .}

  public implementation all x,y ~~ hash[x],hash[y] |: hash[either[x,y]] => {.
    hash(either(A)) => hash(A)*37.
    hash(other(B)) => hash(B)*41.
  .}
}
