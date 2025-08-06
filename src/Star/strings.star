star.strings{
  import star.core.
  import star.arith.
  import star.coerce.
  import star.iterable.

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
    _format(S,Fmt) => _chr_format(S,Fmt).
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
    _format(S,Fmt) => _str_format(S,Fmt).
  }

  public implementation sizeable[string] => {
    size(S) => _str_len(S).
    isEmpty(S) => size(S)==0.
  }

  public implementation  measured[string->>integer] => {
    [|L|] => _str_len(L)
  }

  public implementation indexed[string ->> integer,char] => {
    _index(S,K) => _str_charat(S,K).
    _put(S,K,C) => _str_set(S,K,C).
    _remove(S,K) => _str_drop(S,K).
    _empty = "".
  }.


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
    _coerce(S) => .some(_explode(S)).
  }

  public implementation coercion[cons[char],string] => {
    _coerce(L) => .some(_implode(L)).
  }

  public implementation coercion[string,string] => {
    _coerce(S) => .some(S).
  }

  public implementation coercion[integer,char] => {
    _coerce(S) => .some(_int2chr(S)).
  }

  public implementation coercion[char,integer] => {
    _coerce(S) => .some(_codePoint(S)).
  }

  public implementation coercion[char,string] => {
    _coerce(C) => .some(_implode(.cons(C,.nil))).
  }
  
  public stringQuote:(string)=>string.
  stringQuote(S) => _str_quote(S).

  -- Stream and sequence contracts

  public implementation stream[string->>char] => {
    _eof(S) => S=="".
    _hdtl(Cs) => _str_hdtl(Cs).
  }

  public implementation sequence[string->>char] => {
    _cons(C,S) => _str_cons(C,S).
    _nil = "".
  }

  public implementation iter[string->>char] => let{.
    strIter(S,P,P,X,F) => X.
    strIter(S,P,L,X,F) where P<L && Ch?=_str_charat(S,P) =>
      strIter(S,P+1,L,F(Ch,X),F).
    strIter(_,_,_,X,_) => X.
  .} in {
    _iter(Txt,X,F) => strIter(Txt,0,size(Txt),X,F)
  }

  public implementation generate[string->>char] => {
    _generate(S) => iterGenerator(S)
  }

  public isDigit:(char)=>boolean.
  isDigit(D) => _isNdChar(D).

  public digitVal:(char)=>integer.
  digitVal(`0`) => 0.
  digitVal(`1`) => 1.
  digitVal(`2`) => 2.
  digitVal(`3`) => 3.
  digitVal(`4`) => 4.
  digitVal(`5`) => 5.
  digitVal(`6`) => 6.
  digitVal(`7`) => 7.
  digitVal(`8`) => 8.
  digitVal(`9`) => 9.

  public digitChar:(integer)=>char.
  digitChar(Ch) => case Ch in {
    | 0=>`0`
    | 1=>`1`
    | 2=>`2`
    | 3=>`3`
    | 4=>`4`
    | 5=>`5`
    | 6=>`6`
    | 7=>`7`
    | 8=>`8`
    | 9=>`9`
  }

  public isHexDigit:(char) => option[integer].
  isHexDigit(Ch) => (isDigit(Ch) ?? .some(digitVal(Ch)) ||
    case Ch in {
      | `a` => .some(10)
      | `b` => .some(11)
      | `c` => .some(12)
      | `d` => .some(13)
      | `e` => .some(14)
      | `f` => .some(15)
      | `A` => .some(10)
      | `B` => .some(11)
      | `C` => .some(12)
      | `D` => .some(13)
      | `E` => .some(14)
      | `F` => .some(15)
      | _ default => .none
    }).

  public isSpace:(char) => boolean.
  isSpace(Ch) => (_isZsChar(Ch) || _isZlChar(Ch) || Ch==`\n` || Ch==`\t`).

  public isLetter:(char) => boolean.
  isLetter(Ch) => _isLetterChar(Ch).

  public isAlphaNum:(char) => boolean.
  isAlphaNum(Ch) => (_isLetterChar(Ch) || _isNdChar(Ch)).

  public genSym:(string) => string.
  genSym(Pre) => _str_gen(Pre).

  public strFind:(string,string,integer) => option[integer].
  strFind(Txt,Ky,Ix) where Lc.= _str_find(Txt,Ky,Ix) && Lc>=0 => .some(Lc).
  strFind(_,_,_) default => .none.

  public subString:(string,integer,integer)=>string.
  subString(Txt,Fr,Ln) => _sub_str(Txt,Fr,Ln).

  public strPrefix:(string,string) => boolean.
  strPrefix(Pr,Txt) => _str_start(Pr,Txt).

  public explode:(string)=>cons[char].
  explode(S) => _explode(S).
}
