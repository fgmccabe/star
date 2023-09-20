test.p1{
  import star.
  import star.parse.
  import star.assert.

  a:parser[cons[char],char].
  -- a = _item.
  a = _item >>= (C) => return C.

  p:parser[cons[char],(char,char)].
  p = _item >>= (C) =>
      _item >>= (_) =>
      _item >>= (D) =>
      return (C,D).

  q:parser[cons[char],()].
  q = _tk(`(`) >>= (_) => _tk(`)`) >>= (_) => return ().

  listMem:all e ~~ equality[e] |: (e,cons[e])=>boolean.
  listMem(E,L) => E .<. L.

  symb:(string)=>parser[cons[char],()].
  symb(S) => _str(S).

  -- Simple expression parser
  expr : ()=>parser[cons[char],integer].
  expr() => chainl1(term(),addop()).

  term: ()=>parser[cons[char],integer].
  term() => chainl1(factor(),mulop()).

  factor: ()=>parser[cons[char],integer].
  factor() => decimal() +++ (symb("(") >>= (_) => expr() >>= (F) => symb(")") >>= (_) => return F).

  addop: ()=>parser[cons[char],(integer,integer)=>integer].
  addop() => (symb("+") >>= (_) => return (+)) +++ (symb("-") >>= (_) => return (-)).

  mulop: ()=>parser[cons[char],(integer,integer)=>integer].
  mulop() => (symb("*") >>= (_) => return (*)).

  decimal:()=>parser[cons[char],integer].
  decimal() => skip(digit()) >>= (D) => return (digitVal(D)).

  digit:()=>parser[cons[char],char].
  digit() => _sat(isDigit).

  main:() => ().
  main() => valof{
    assert parse(p,[`1`,`2`,`3`]) == [((`1`,`3`),[])];

    show parse(a,"1"::cons[char]);

    assert parse(q,[`(`,`)`]) == [((),[])];

    assert parse(_str("alpha"),"alpha0"::cons[char]) == [((),[`0`])];

    assert listMem((([(),()]:cons[()]),[]),parse(_plus(_str("a")),"aa"::cons[char]));

    show parse(_star(_str("a")),"aab"::cons[char]);

    assert listMem((([(),()]),[`b`]),parse(_star(_str("a")),"aab"::cons[char]));

    assert parse(expr(),"(3+5*3)"::cons[char]) == [(18,[])];
    valis ()
  }
}
