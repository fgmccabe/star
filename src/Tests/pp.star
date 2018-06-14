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
  factor --> decimal.
  factor^F --> "(", expr^F, ")".

  addop: parser[integer,(integer,integer)=>integer].
  addop^(+) --> "+".
  addop^(-) --> "-".

  mulop:parser[integer,(integer,integer)=>integer].
  mulop^(*) --> "*".
  mulop^(/) --> "/".

  decimal:parser[integer,integer].
  decimal^(D-0c0) --> space*,digit^D.

  decimal --> space*, digit->D, returns E

  digit:parser[integer,integer].
  digit = sat(isDigit).

  assert parse(expr,"(3+5*3)"::list[integer]) == [(18,[])].
}
