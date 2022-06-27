star.either{
  import star.core.
  import star.arith.
  import star.cons.

  -- Either or values
  public all a,b ~~ either[b,a] ::= either(a) | other(b).

  -- Display either-or values
  public implementation all x,y ~~ display[x], display[y] |: display[either[x,y]] => {
    disp(either(a)) => "either($(a))".
    disp(other(b)) => "or($(b))".
  }

  public implementation all x,y ~~ equality[x], equality[y] |: equality[either[x,y]] => {
    either(A)==either(B) => A==B.
    other(A)==other(B) => A==B.
    _==_ => .false.
  }

  public implementation all x,y ~~ hashable[x],hashable[y] |: hashable[either[x,y]] => {
    hash(either(A)) => hash(A)*37.
    hash(other(B)) => hash(B)*41.
  }
}
