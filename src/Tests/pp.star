test.pp{
  import star.
  import star.parse.

  -- Expression grammar using the grammar rule annotation
  -- Simple expression parser

  expr : parser[integer,integer].
  expr = chainl1(term,addop).

  term: parser[integer,integer].
  term = chainl1(factor,mulop).

  factor:parser[integer,integer].
  factor = prse{ decimal || "("; F<-expr; ")" ^^F }.

  addop: parser[integer,(integer,integer)=>integer].
  addop = prse{ "+" ^^ (+) || "-" ^^ (-) }.

  mulop:parser[integer,(integer,integer)=>integer].
  mulop = prse{"*" ^^ (*) || "/" ^^ (/) }.

  decimal:parser[integer,integer].
  decimal = prse{space*,D<-digit ^^ (D-0c0)}.

  digit:parser[integer,integer].
  digit = _sat(isDigit).

  assert parse(expr,"(3+5*3)"::list[integer]) == [(18,[])].
}
