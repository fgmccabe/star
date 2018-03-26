star.strings{
  import star.core.
  import star.sequence.

  -- and strings ...
  public implementation equality[string] => {
    X == Y => _str_eq(X,Y).
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
  displayString(S) => ssSeq([sc(0c\"),ssSeq(quoteStr(explode(S))),sc(0c\")]).

  private quoteStr:(list[integer]) => list[ss].
  quoteStr([])=>[].
  quoteStr([c,..l]) => qtChr(c,quoteStr(l)).

  private qtChr:(integer,list[ss]) => list[ss].
  qtChr(0c",l) => [sc(0c\\),sc(0c"),..l].
  qtChr(0c\\,l) => [sc(0c\\),sc(0c\\),..l].
  qtChr(c,l) => [sc(c),..l].

  public implementation sizeable[string] => {
    size(S) => _str_len(S).
    isEmpty("").
  }

}
