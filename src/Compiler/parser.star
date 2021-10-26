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

  public parseSrc:(uri,pkg,reports) => (option[ast],reports).
  parseSrc(U,P,Rp) where Txt ^= getResource(U) &&
      (Toks,Rp1) .= allTokens(initSt(pkgLoc(P),Txt::cons[char],Rp)) =>
    (errorFree(Rp1) ?
	((Trm,Rptx,_) .= astParse(Toks,Rp1) ?
	    (errorFree(Rptx) ?
		(some(Trm),Rptx) ||
		(.none,Rptx)) ||
	    (.none,Rp1)) ||
	(.none,Rp1)).
  parseSrc(U,P,Rp) => (.none,reportError(Rp,"Cannot locate $(P) in $(U)",pkgLoc(P))).

  public parseText:(locn,string,reports) => (option[ast],reports).
  parseText(Lc,Txt,Rpt) =>
    ( ((Toks,Rp1).=allTokens(initSt(Lc,Txt::cons[char],Rpt))) &&
	  (Trm,Rptx,_) .= astParse(Toks,Rp1)) ?
      (some(Trm),Rptx) ||
      (.none, reportError(Rpt,"Could not successfully parse",Lc)).
}
