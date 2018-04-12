star.option{
  import star.core.
  import star.arith.
  import star.lists.
  import star.strings.

  option@"the option type is useful when a value is not always available".
  public all t ~~ option[t] ::= none | some(t).

  -- display optional values
  public implementation all x ~~ display[x] |: display[option[x]] => {
    disp(O) => dispOptional(O).
  }

  private dispOptional:all x ~~ display[x] |: (option[x]) => ss.
  dispOptional(none) => ss("none").
  dispOptional(some(X)) => ssSeq([ss("some("),disp(X),ss(")")]).

  public implementation all x ~~ equality[x] |: equality[option[x]] => {
    X == Y => optionEqual(X,Y).
  }

  private optionEqual:all x ~~ equality[x] |: (option[x],option[x]) => boolean.
  optionEqual(some(A),some(B)) => A==B.
  optionEqual(none,none) => true.
  optionEqual(_,_) => false.

  public implementation all x ~~ hash[x] |: hash[option[x]] => {.
    hash(some(X)) => hash("some")*37+hash(X).
    hash(none) => hash("none").
  .}

  public maybe:all x ~~ ((x)=>boolean) => option[x].
  maybe(P) where P(x) => some(x).
  maybe(_) => none.
}
