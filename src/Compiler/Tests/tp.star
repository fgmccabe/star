test.comp.parsetype{
  import star.

  import star.compiler.dict.
  import star.compiler.typeparse.
  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.opg.
  import star.compiler.parser.
  import star.compiler.token.
  import star.compiler.lexer.
  import star.compiler.location.
  import star.compiler.types.
  import star.pkg.

  R0 = reports([]).
  lc = startLoc("test").

  getAst((some(A),_)) => A.

  A1 = getAst(parseText(lc,"(integer,string)=>integer. ",R0)).
  T1 = valof parseType([],A1,stdDict,R0).

--  assert T1==funType(tupleType([intType,strType]),intType).

  show disp(T1).

  A2 = getAst(parseText(lc,"all e ~~ (e,integer)=>e. ",R0)).

  T2 = valof parseType([],A2,stdDict,R0).

  assert T2==allType(kVar("e"),funType(tupleType([kVar("e"),intType]),kVar("e"))).

  show disp(T2).

  A3 = getAst(parseText(lc,"all e ~~ (list[e])=>list[e]. ",R0)).
  T3 = valof parseType([],A3,stdDict,R0).

  show disp(T3).

  assert T3==allType(kVar("e"),funType(tupleType([lstType(kVar("e"))]),lstType(kVar("e")))).

  A4 = getAst(parseText(lc,"all e ~~ { get:()=>e. set:(e)=>e. }. ",R0)).
  T4 = valof parseType([],A4,stdDict,R0).
  show disp(T4).

  assert T4==allType(kVar("e"),
    faceType([("set",funType(tupleType([kVar("e")]),kVar("e"))),
	("get",funType(tupleType([]),kVar("e")))],[])).

}
