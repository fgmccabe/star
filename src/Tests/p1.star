test.p1{
  import star.
  import star.parse.
  import star.script.

  a:parser[cons[integer],integer].
  -- a = _item.
  a = _item >>= (C) => return C.

  p:parser[cons[integer],(integer,integer)].
  p = _item >>= (C) =>
      _item >>= (_) =>
      _item >>= (D) =>
      return (C,D).

  q:parser[cons[integer],()].
  q = _tk(0c() >>= (_) => _tk(0c)) >>= (_) => return ().

  listMem:all e ~~ equality[e] |: (e,cons[e])=>boolean.
  listMem(E,L) => _contains(L,E).

  symb:(string)=>parser[cons[integer],()].
  symb(S) => _str(S).

  -- Simple expression parser
  expr : parser[cons[integer],integer].
  expr = chainl1(term,addop).

  term: parser[cons[integer],integer].
  term = chainl1(factor,mulop).

  factor:parser[cons[integer],integer].
  factor = decimal +++ (symb("(") >>= (_) => expr >>= (F) => symb(")") >>= (_) => return F).

  addop: parser[cons[integer],(integer,integer)=>integer].
  addop = (symb("+") >>= (_) => return (+)) +++ (symb("-") >>= (_) => return (-)).

  mulop:parser[cons[integer],(integer,integer)=>integer].
  mulop = (symb("*") >>= (_) => return (*)) +++ (symb("/") >>= (_) => return (/)).

  decimal:parser[cons[integer],integer].
  decimal = skip(digit) >>= (D) => return (D-0c0).

  digit:parser[cons[integer],integer].
  digit = _sat(isDigit).

  main:() => action[(),()].
  main() => do{
    assert parse(p,[1,2,3]) == [((1,3),[])];

    show disp(parse(a,"1"::cons[integer]))::string;

    assert parse(q,[0c(,0c)]) == [((),[])];

    assert parse(_str("alpha"),"alpha0"::cons[integer]) == [((),[0c0])];

    assert listMem((([(),()]:cons[()]),[]),parse(_plus(_str("a")),"aa"::cons[integer]));

    show disp(parse(_star(_str("a")),"aab"::cons[integer]))::string;

    assert listMem((([(),()]),[0cb]),parse(_star(_str("a")),"aab"::cons[integer]));

    assert parse(expr,"(3+5*3)"::cons[integer]) == [(18,[])]
  }
}
