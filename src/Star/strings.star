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

  public implementation coercion[string,string] => {
    _coerce(S) => S.
  }

  public isDigit:(integer)=>boolean.
  isDigit(D) => _isNdChar(D).

  public digitVal:(integer)=>integer.
  digitVal(D) => _digitCode(D).

  public isHexDigit:(integer) => option[integer].
  isHexDigit(Ch) where isDigit(Ch) => some(digitVal(Ch)).
  isHexDigit(0ca) => some(10).
  isHexDigit(0cb) => some(11).
  isHexDigit(0cc) => some(12).
  isHexDigit(0cd) => some(13).
  isHexDigit(0ce) => some(14).
  isHexDigit(0cf) => some(15).
  isHexDigit(0cA) => some(10).
  isHexDigit(0cB) => some(11).
  isHexDigit(0cC) => some(12).
  isHexDigit(0cD) => some(13).
  isHexDigit(0cE) => some(14).
  isHexDigit(0cF) => some(15).
  isHexDigit(_) default => none.

  public isSpace:(integer) => boolean.
  isSpace(Ch) => _isZsChar(Ch).

  public isLetter:(integer) => boolean.
  isLetter(Ch) => _isLetterChar(Ch).

  public isAlphaNum:(integer) => boolean.
  isAlphaNum(Ch) => (_isLetterChar(Ch) || _isNdChar(Ch)).

}
