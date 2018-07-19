_tk_tktkstar.p1{
  import star.
  import star.parse.

  p:parser[integer,(integer,integer)].
  p = _item >>= (C) =>
      _item >>= (_) =>
      _item >>= (D) =>
      return (C,D).

  assert parse(p,[1,2,3]) == [((1,3),[])].

  q:parser[integer,()].
  q = _tk(0c() >>= (_) => _tk(0c)) >>= (_) => return ().

  listMem:all e ~~ equality[e] |: (e,list[e])=>boolean.
  listMem(E,L) => _is_member(E,L,0,_list_size(L)).

  _is_member:all e ~~ equality[e] |: (e,list[e],integer,integer) => boolean.
  _is_member(_,_,Ix,Ix) => false.
  _is_member(E,L,Ix,_) where _list_nth(L,Ix)==E => true.
  _is_member(E,L,Ix,Lx) => _is_member(E,L,Ix+1,Lx).

  assert parse(q,[0c(,0c)]) == [((),[])].

  assert parse(_str("alpha"),"alpha0"::list[integer]) == [((),[0c0])].

  assert listMem((([(),()]:list[()]),[]),parse(_plus(_str("a")),"aa"::list[integer])).

  show disp(parse(_star(_str("a")),"aab"::list[integer]))::string.

  assert listMem((([(),()]),[0cb]),parse(_star(_str("a")),"aab"::list[integer])).

  symb:(string)=>parser[integer,()].
  symb(S) => _str(S).

  -- Simple expression parser
  expr : parser[integer,integer].
  expr = chainl1(term,addop).

  term: parser[integer,integer].
  term = chainl1(factor,mulop).

  factor:parser[integer,integer].
  factor = decimal +++ (symb("(") >>= (_) => expr >>= (F) => symb(")") >>= (_) => return F).

  addop: parser[integer,(integer,integer)=>integer].
  addop = (symb("+") >>= (_) => return (+)) +++ (symb("-") >>= (_) => return (-)).

  mulop:parser[integer,(integer,integer)=>integer].
  mulop = (symb("*") >>= (_) => return (*)) +++ (symb("/") >>= (_) => return (/)).

  decimal:parser[integer,integer].
  decimal = skip(digit) >>= (D) => return (D-0c0).

  digit:parser[integer,integer].
  digit = _sat(isDigit).

  assert parse(expr,"(3+5*3)"::list[integer]) == [(18,[])].

}
