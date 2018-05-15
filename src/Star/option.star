star.option{
  import star.core.
  import star.arith.
  import star.lists.
  import star.strings.
  import star.monad.

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

  public implementation all e ~~ monad[option] => {
    return x => some(x).
    (some(x) >>= f) => f(x).
    (none >>= _) => none.
  }

  public implementation execution[option->>()] => {
    _raise(_) => none.
    _perform(some(X)) => X.
    _handle(some(X),_) => some(X).
    _handle(none,E) => E(()).
  }
}
