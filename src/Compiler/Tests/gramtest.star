test.comp.gram{
  import star.
  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.opg.
  import star.compiler.parser.
  import star.compiler.token.
  import star.compiler.lexer.
  import star.compiler.location.
  import star.pkg.

  lc=pkgLoc(pkg("test",defltVersion)).

  show disp(parseText(lc,"X. ",reports([])))::string.

  assert (some(nme(_,"X")),_).=parseText(lc,"X. ",reports([])).

  assert (some(app(_,nme(_,"+"),tpl(_,"()",[lit(_,intgr(2)),lit(_,intgr(3))]))),_) .=
    parseText(lc,"2+3. ",reports([])).

  show disp(parseText(lc,"(X)=>X+Y*Z. ",reports([])))::string.

  show disp(parseText(lc,"f(X)(Y)=>X+Y*Z. ",reports([])))::string.

  show disp(parseText(lc,"{f(X)(Y)=>X+Y*Z}",reports([])))::string.

  show disp(parseText(lc,"{f(X)=>g(Y). g(Y)=>(X+Y)*Z}",reports([])))::string.
}
