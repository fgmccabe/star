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

  public parseSrc:(uri,pkg,reports) => either[reports,ast].
  parseSrc(U,P,Rp) => do{
    if Txt ^= getResource(U) then{
--      logMsg("parse $(U)");
      Chrs .= Txt::cons[integer];
--      logMsg("got chars");
      Toks .= allTokens(initSt(pkgLoc(P),Chrs));
--      logMsg("input tokens are $(Toks)");
      (Trm,Rp1,_) .= astParse(Toks,Rp);
--      logMsg("input term $(Trm)");
      if errorFree(Rp1) then
	valis Trm
      else
      throw reportError(Rp1,"could not parse $(P) properly",pkgLoc(P))
    } else
    throw reportError(Rp,"Cannot locate $(P) in $(U)",pkgLoc(P))
  }

  public parseText:(locn,string,reports) => (option[ast],reports).
  parseText(Lc,Txt,Rpt) =>
    ( (Toks.=allTokens(initSt(Lc,Txt::cons[integer])) &&
      (Trm,Rptx,_) .= astParse(Toks,Rpt)) ?
        (some(Trm),Rptx) ||
        (.none, reportError(Rpt,"Could not successfully parse",Lc))).

  public tokenizeSrc:(uri,pkg,reports) => either[reports,cons[token]].
  tokenizeSrc(U,P,Rp) => do{
    if Txt ^= getResource(U) then{
--      logMsg("parse $(U)");
      valis allTokens(initSt(pkgLoc(P),Txt::cons[integer]))
--      logMsg("input tokens are $(Toks)");
    } else
    throw reportError(Rp,"Cannot locate $(P) in $(U)",pkgLoc(P))
  }
}
