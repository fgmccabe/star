star.compiler.parser{
  -- top level of star parser
  import star.
  import star.pkg.
  import star.resources.
  import star.uri.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.lexer.
  import star.compiler.location.
  import star.compiler.opg.
  import star.compiler.token.

  public parseSrc:(uri,pkg) => option[ast].
  parseSrc(U,P) where Txt ?= getResource(U) &&
      (Toks) .= allTokens(initSt(pkgLoc(P),Txt::cons[char])) =>
    (errorFree() ??
      ((Trm,_) .= astParse(Toks) ??
	(errorFree() ??
	  .some(Trm) ||
	  .none) ||
	.none) ||
      .none).
  parseSrc(U,P) => valof{
    reportError("Cannot locate $(P) in $(U)",.some(pkgLoc(P)));
    valis .none
  }
}
