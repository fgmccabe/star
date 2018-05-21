star.p1{
  import star.
  import star.parse.

  p:parser[integer,(integer,integer)].
  p = item >>= (C) =>
      item >>= (_) =>
      item >>= (D) =>
      return (C,D).

  assert parse(p,[1,2,3]) == [((1,3),[])].

  q:parser[integer,()].
  q = tk(0c() >>= (_) => tk(0c)) >>= (_) => return ().

  (in):all e ~~ equality[e] |: (e,list[e])=>boolean.
  E in L => _is_member(E,L,0,_list_size(L)).

  _is_member:all e ~~ equality[e] |: (e,list[e],integer,integer) => boolean.
  _is_member(_,_,Ix,Ix) => false.
  _is_member(E,L,Ix,_) where _list_nth(L,Ix)==E => true.
  _is_member(E,L,Ix,Lx) => _is_member(E,L,Ix+1,Lx).

  assert parse(q,[0c(,0c)]) == [((),[])].

  assert parse(str("alpha"),"alpha0"::list[integer]) == [((),[0c0])].

  assert (([(),()]:list[()]),[]) in parse(many(str("a")),"aa"::list[integer]).

  assert parse(many(str("a")),"aab"::list[integer]) == [(([(),()]),[0cb])].

  symb:(string)=>parser[integer,()].
  symb(S) => str(S).

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
  digit = sat(isDigit).

  assert parse(expr,"(3+5*3)"::list[integer]) == [(18,[])].

}
