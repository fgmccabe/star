star.strings{
  import star.core.
  import star.coerce.

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

  public implementation coercion[string,list[integer]] => {
    _coerce(S) => _explode(S).
  }

  public isDigit:(integer)=>boolean.
  isDigit(D) => _isNdChar(D).

  public isSpace:(integer) => boolean.
  isSpace(Ch) => _isZsChar(Ch).

  public isLetter:(integer) => boolean.
  isLetter(Ch) => _isLetterChar(Ch).

}
