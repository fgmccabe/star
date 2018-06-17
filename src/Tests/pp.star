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
  factor --> decimal || "(", F<-expr, ")" ^^F.

  addop: parser[integer,(integer,integer)=>integer].
  addop --> "+" ^^ (+) || "-" ^^ (-).

  mulop:parser[integer,(integer,integer)=>integer].
  mulop --> "*" ^^ (*) || "/" ^^ (/).

  decimal:parser[integer,integer].
  decimal --> space*,D<-digit ^^ (D-0c0).

  digit:parser[integer,integer].
  digit = sat(isDigit).

  assert parse(expr,"(3+5*3)"::list[integer]) == [(18,[])].
}
