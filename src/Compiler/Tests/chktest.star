test.comp.chk{
  import star.
  import star.pkg.

  import star.compiler.location.
  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.checker.
  import star.compiler.dict.
  import star.compiler.dependencies.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.parser.
  import star.compiler.types.

  R0 = reports([]).
  Pk = pkg("test",defltVersion).
  lc = pkgLoc(Pk).

  getAst((some(A),_)) => A.

  getBrEls(A) where (_,Els) ?= isBrTuple(A) => Els.

  S1 = """
{
--  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>_int_times(N,fact(_int_minus(N,1))).
}
""".

  A1 = getAst(parseText(lc,S1,R0)).

  show disp(A1).

  SD = declareVar("true",none,tipe("star.core*boolean"),stdDict).

  P1 = checkProgram(getBrEls(A1),"test",SD,R0).

  show disp(P1).
  
}
