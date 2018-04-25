star.strings{
  import star.core.

  -- and strings ...
  public implementation equality[string] => {
    X==Y => _str_eq(X,Y).
  }

  public implementation hash[string] => {
    hash(X) => _str_hash(X).
  }

  public implementation comp[string] => {
    X<Y => _str_lt(X,Y).
    X>=Y => _str_ge(X,Y).
  }

  public implementation display[string] => {
    disp(X) => displayString(X).
  }

  public displayString:(string) => ss.
  displayString(S) => ss(_stringOf(S,1)).

  public implementation sizeable[string] => {
    size(S) => _str_len(S).
    isEmpty("") => true.
    isEmpty(_) => false.
  }

  public implementation concat[string] => {
    S1++S2 => _str_concat(S1,S2).
  }
}
