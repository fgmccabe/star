star.strings{
  import star.core.
  import star.arith.
  import star.coerce.

  -- and strings ...
  public implementation equality[string] => {
    X==Y => _string_eq(X,Y).
  }

  public implementation equality[chars] => {
    X==Y => _str_eq(X,Y).
  }

  public implementation hash[string] => {
    hash(X) => _str_hash(_str_fltn(X)).
  }

  public implementation comp[string] => {
    X<Y => _str_lt(_str_fltn(X),_str_fltn(Y)).
    X>=Y => _str_ge(_str_fltn(X),_str_fltn(Y)).
  }

  public implementation display[string] => {.
    disp(S) => "\"#(chrs_(_str_quote(_str_fltn(S))))\"".
  .}

  public implementation display[chars] => {.
    disp(S) => "\"0#(chrs_(_str_quote(S)))\"".
  .}

  public implementation format[string] => {.
    frmt(S,Fmt) => chrs_(_str_format(_str_fltn(S),Fmt)).
  .}

  public implementation sizeable[string] => {
    size(S) => _string_len(S).
    isEmpty(S) => size(S)==0.
  }

  public implementation  measured[string->>integer] => {.
    [|L|] => _string_len(L)
  .}

  public implementation concat[string] => {
    S1++S2 => pair_(S1,S2).
    _multicat(.nil) => "".
    _multicat(cons(S,Ss)) => pair_(S,_multicat(Ss)).
  }.

  public implementation slice[string->>integer] => {
    _slice(S,F,T) => chrs_(_sub_str(_str_fltn(S),F,T-F)).
    _splice(S,F,T,N) => chrs_(_str_splice(_str_fltn(S),F,T-F,_str_fltn(N))).
  }.

  public implementation reversible[string] => {
    reverse(L) => chrs_(_str_reverse(_str_fltn(L))).
  }

  public implementation coercion[string,cons[integer]] => {
    _coerce(S) => some(_explode(_str_fltn(S))).
  }

  public implementation coercion[cons[integer],string] => {
    _coerce(L) => some(chrs_(_implode(L))).
  }

  public implementation coercion[string,string] => {
    _coerce(S) => some(S).
  }

  public stringQuote:(string)=>string.
  stringQuote(S) => chrs_(_str_quote(_str_fltn(S))).

  -- Stream and sequence contracts

  public implementation stream[string->>integer] => {
    _eof(S) => S=="".
    _hdtl(chrs_(Cs)) where (H,T).=_str_hdtl(Cs) => some((H,chrs_(T))).
    _hdtl(pair_(L,R)) where (H,T) ^= _hdtl(L) => some((H,pair_(T,R))).
    _hdtl(chrs_(0"")) => .none.
  }

  public implementation sequence[string->>integer] => {.
    _cons(C,S) => pair_(chrs_(_code2str(C)),S).
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

  public genSym:(string) => string.
  genSym(Pre) => chrs_(_str_gen(_str_fltn(Pre))).

  public strFind:(string,string,integer) => integer.
  strFind(Txt,Ky,Ix) => _str_find(_str_fltn(Txt),_str_fltn(Ky),Ix).

  public subString:(string,integer,integer)=>string.
  subString(Txt,Fr,Ln) => chrs_(_sub_str(_str_fltn(Txt),Fr,Ln)).
}
