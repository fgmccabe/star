test.opg{
  import star.
  import star.parse.
  import test.ops.
  import test.ast.

  term:(integer)=>parser[ast].
  term(Pr) => termLeft(Pr) >>= ((Lft,LPr)) => termRight((Lft,LPr,Pr)).

  termLeft:(integer) => parser[(ast,integer)].
  termLeft(Pr) =>  



}
