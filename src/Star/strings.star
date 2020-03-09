star.strings{
  import star.core.
  import star.arith.
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
    isEmpty("") => .true.
    isEmpty(_) => .false.
  }

  public implementation concat[string] => {
    S1++S2 => _str_concat(S1,S2).
  }.

  public implementation slice[string->>integer] => {
    _slice(S,F,T) => _sub_str(S,F,T-F).
    _splice(S,F,T,N) => _str_splice(S,F,T-F,N).
  }.

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

  -- Stream and sequence contracts

  public implementation stream[string->>integer] => {.
    _eof(S) => S=="".
    _hdtl(S) default => some(_str_hdtl(S)).
    _hdtl("") => .none.

    _back("") => .none.
    _back(S) => some(_str_back(S)).
  .}

  public implementation sequence[string->>integer] => {.
    _cons(C,S) => _str_cons(C,S).

    _apnd(S,C) => _str_apnd(S,C).
    _nil = "".
  .}

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
  isHexDigit(_) default => .none.

  public isSpace:(integer) => boolean.
  isSpace(Ch) => (_isZsChar(Ch) || _isZlChar(Ch) || Ch==0c\n || Ch==0c\t).

  public isLetter:(integer) => boolean.
  isLetter(Ch) => _isLetterChar(Ch).

  public isAlphaNum:(integer) => boolean.
  isAlphaNum(Ch) => (_isLetterChar(Ch) || _isNdChar(Ch)).

}
