star.strings{
  import star.core.
  import star.arith.
  import star.coerce.

  -- Chars --
  public implementation equality[char] => {
    X==Y => _chr_eq(X,Y).
  }

  public implementation comp[char] => {
    X<Y => _chr_lt(X,Y).
    X>=Y => _chr_ge(X,Y).
  }

  public implementation display[char] => {
    disp(Cp) => "`#(_chr_quote(Cp))`".
  }

  public implementation format[char] => {
    frmt(S,Fmt) => _chr_format(S,Fmt).
  }

  -- and strings ...
  public implementation equality[string] => {
    X==Y => _str_eq(X,Y).
  }

  public implementation hashable[char] => {
    hash(X) => _chr_hash(X).
  }

  public implementation hashable[string] => {
    hash(X) => _str_hash(X).
  }

  public implementation comp[string] => {
    X<Y => _str_lt(X,Y).
    X>=Y => _str_ge(X,Y).
  }

  public implementation display[string] => {
    disp(S) => "\"#(_str_quote(S))\"".
  }

  public implementation format[string] => {
    frmt(S,Fmt) => _str_format(S,Fmt).
  }

  public implementation sizeable[string] => {
    size(S) => _str_len(S).
    isEmpty(S) => size(S)==0.
  }

  public implementation  measured[string->>integer] => {
    [|L|] => _str_len(L)
  }

  public implementation concat[string] => {
    S1++S2 => _str_concat(S1,S2).
    _multicat(Els) => _str_multicat(Els).
  }.

  public implementation slice[string->>integer] => {
    _slice(S,F,T) => _sub_str(S,F,T-F).
    _splice(S,F,T,N) => _str_splice(S,F,T-F,N).
  }.

  public implementation reversible[string] => {
    reverse(L) => _str_reverse(L).
  }

  public implementation coercion[string,cons[char]] => {
    _coerce(S) => ?_explode(S).
  }

  public implementation coercion[cons[char],string] => {
    _coerce(L) => ?_implode(L).
  }

  public implementation coercion[string,string] => {
    _coerce(S) => ?S.
  }

  public implementation coercion[integer,char] => {
    _coerce(S) => ?_char(S).
  }

  public implementation coercion[char,integer] => {
    _coerce(S) => ?_codePoint(S).
  }
  
  public stringQuote:(string)=>string.
  stringQuote(S) => _str_quote(S).

  -- Stream and sequence contracts

  public implementation stream[string->>char] => {
    _eof(S) => S=="".
    _hdtl(Cs) where (H,T).=_str_hdtl(Cs) => ?(H,T).
    _hdtl("") => .none.
  }

  public implementation sequence[string->>char] => {
    _cons(C,S) => _str_cons(C,S).
    _nil = "".
  }

  public isDigit:(char)=>boolean.
  isDigit(D) => _isNdChar(D).

  public digitVal:(char)=>integer.
  digitVal(D) => _digitCode(D).

  public digitChar:(integer)=>char.
  digitChar(Ch) => case Ch in {
    0=>`0`.
    1=>`1`.
    2=>`2`.
    3=>`3`.
    4=>`4`.
    5=>`5`.
    6=>`6`.
    7=>`7`.
    8=>`8`.
    9=>`9`.
  }

  public isHexDigit:(char) => option[integer].
  isHexDigit(Ch) => isDigit(Ch) ? ?digitVal(Ch) ||
    case Ch in {
      `a` => ?10.
      `b` => ?11.
      `c` => ?12.
      `d` => ?13.
      `e` => ?14.
      `f` => ?15.
      `A` => ?10.
      `B` => ?11.
      `C` => ?12.
      `D` => ?13.
      `E` => ?14.
      `F` => ?15.
      _ default => .none.
    }

  public isSpace:(char) => boolean.
  isSpace(Ch) => (_isZsChar(Ch) || _isZlChar(Ch) || Ch==`\n` || Ch==`\t`).

  public isLetter:(char) => boolean.
  isLetter(Ch) => _isLetterChar(Ch).

  public isAlphaNum:(char) => boolean.
  isAlphaNum(Ch) => (_isLetterChar(Ch) || _isNdChar(Ch)).

  public genSym:(string) => string.
  genSym(Pre) => _str_gen(Pre).

  public strFind:(string,string,integer) => option[integer].
  strFind(Txt,Ky,Ix) where Lc.= _str_find(Txt,Ky,Ix) && Lc>=0 => ?Lc.
  strFind(_,_,_) default => .none.

  public subString:(string,integer,integer)=>string.
  subString(Txt,Fr,Ln) => _sub_str(Txt,Fr,Ln).
}
