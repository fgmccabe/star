test.comp.depends{
  import star.

  import star.compiler.location.
  import star.compiler.ast.
  import star.compiler.dependencies.
  import star.compiler.errors.
  import star.compiler.meta.
  import star.compiler.parser.

  import star.pkg.

  R0 = reports([]).
  lc = pkgLoc(pkg("test",defltVersion)).

  getAst((some(A),_)) => A.

  S1 = """
{
  import star.

  public fact:(integer)=>integer.
  fact(0)=>1.
  fact(N)=>N*fact(N-1).

  assert fact(3)==6.
}
""".

  A1 = getAst(parseText(lc,S1,R0)).

  show disp(A1).

  getA:all a,b ~~ (option[(a,b)]) => a.
  getA(some((A,_))) => A.
  
  getB:all a,b ~~ (option[(a,b)]) => b.
  getB(some((_,B))) => B.

  A11 = getB(isBrTuple(A1)).

  show disp(A11).

  D1 = dependencies(A11,R0).

  show disp(D1).

  S2 = """
  {
  import star.

    inc:(integer)=>integer.
    inc(X) => X+1.

    df(X)=>inc(f(X)).

  f:(integer)=>integer.
  f(0)=>1.
  f(N)=>N*g(N-1).

  g:(integer)=>integer.
  g(0)=>1.
  g(N)=>N*f(N-1).

  fact(X) => f(X).

  assert fact(3)==6.
  }
  """.

  D2 = dependencies(getB(isBrTuple(getAst(parseText(lc,S2,R0)))),R0).

  show disp(D2).
 
}
