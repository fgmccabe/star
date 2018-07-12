test.comp.gram{
  import star.
  import star.compiler.ast.
  import star.compiler.opg.
  import star.compiler.token.
  import star.compiler.lexer.
  import star.compiler.location.
  import star.pkg.

  p:pkg.
  p=pkg("test",defltVersion).

  st:tokenState.
  st = initSt(p,"(X)=>X+Y*Z. "::list[integer]).

  show disp(allTokens(st))::string.

  show disp(parse(allTokens(st),report([])))::string.
}
