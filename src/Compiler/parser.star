star.compiler.parser{
  -- top level of star parser
  import star.
  import star.parse.
  import star.pkg.
  import star.resources.
  import star.uri.

  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.lexer.
  import star.compiler.location.
  import star.compiler.opg.
  import star.compiler.token.

  public parseSrc:(uri,pkg,reports) => (option[ast],reports).
  parseSrc(U,P,Rpt) where
    Txt ^= getResource(U) =>
      ( (Toks.=allTokens(initSt(P,Txt::list[integer])) &&
        ((Trm,Rptx),_) ^= head(parse(astParser(Rpt),Toks))) ?
          (some(Trm),Rptx) ||
          (none, reportError(Rpt,"Could not successfully parse \(P)",pkgLoc(P))))
  parseSrc(U,P,Rpt) default => (none,reportError(Rpt,"Cannot locate \(P) in \(U)",pkgLoc(P))).

}
