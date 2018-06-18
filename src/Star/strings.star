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

  public implementation reversible[string] => {
    reverse(L) => _str_reverse(L).
  }

  public implementation coercion[string,list[integer]] => {
    _coerce(S) => _explode(S).
  }

  public implementation coercion[list[integer],string] => {
    _coerce(L) => _implode(L).
  }

  public isDigit:(integer)=>boolean.
  isDigit(D) => _isNdChar(D).

  public digitVal:(integer)=>integer.
  digitVal(D) => _digitCode(D).

  public isHexDigit:(integer)=>boolean.
  isHexDigit(0ca) => true.
  isHexDigit(0cb) => true.
  isHexDigit(0cc) => true.
  isHexDigit(0cd) => true.
  isHexDigit(0ce) => true.
  isHexDigit(0cf) => true.
  isHexDigit(0cA) => true.
  isHexDigit(0cB) => true.
  isHexDigit(0cC) => true.
  isHexDigit(0cD) => true.
  isHexDigit(0cE) => true.
  isHexDigit(0cF) => true.
  isHexDigit(Ch) => isDigit(Ch).

  public isSpace:(integer) => boolean.
  isSpace(Ch) => _isZsChar(Ch).

  public isLetter:(integer) => boolean.
  isLetter(Ch) => _isLetterChar(Ch).

  public isAlphaNum:(integer) => boolean.
  isAlphaNum(Ch) => (_isLetterChar(Ch) || _isNdChar(Ch)).

}
