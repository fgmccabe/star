star.either{
  import star.core.
  import star.arith.
  import star.cons.

  -- Either or values
  public all a,b ~~ either[a,b] ::= .either(a) | .neither | .other(b).

  -- Display either-or values
  public implementation all x,y ~~ display[x], display[y] |: display[either[x,y]] => {
    disp(.either(a)) => "either($(a))".
    disp(.other(b)) => "or($(b))".
    disp(.neither) => "neither".
  }

  public implementation all x,y ~~ equality[x], equality[y] |: equality[either[x,y]] => {
    .either(A)==.either(B) => A==B.
    .other(A)==.other(B) => A==B.
    .neither == .neither => .true.
    _==_ => .false.
  }

  public implementation all x,y ~~ hashable[x],hashable[y] |: hashable[either[x,y]] => {
    hash(.either(A)) => hash(A)*37.
    hash(.other(B)) => hash(B)*41.
    hash(.neither) => 0.
  }

  -- public implementation all e,o ~~ raises o |: pull[either[e,o]->>e] => {
  --   ? .either(A) => A.
  --   ? .other(B) => raise B
  -- }
}
