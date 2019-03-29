test.comp.lx{
  import star.
  import star.compiler.lexer.
  import star.compiler.token.
  import star.compiler.location.
  import star.pkg.

  lc=pkgLoc(pkg("test",defltVersion)).

  st:tokenState.
  st = initSt(lc,"  /* comment */\n-- comment\n 0c9 "::list[integer]).

  show(disp(nextToken(initSt(lc,"\n0c9"::list[integer])))::string).

  show(disp(nextToken(st))::string).

  assert (_,tok(_,intTok(57))) ^= nextToken(st).

  show(disp(nextToken(initSt(lc,"/*hex*/0x64"::list[integer])))::string).

  assert (_,tok(_,intTok(100))) ^= nextToken(initSt(lc,"/*hex*/0x64"::list[integer])).

  show(disp(nextToken(initSt(lc,"/*int*/64"::list[integer])))::string).
  assert (_,tok(_,intTok(64))) ^= nextToken(initSt(lc,"/*int*/64"::list[integer])).

  show(disp(nextToken(initSt(lc,"/*flt*/64.0"::list[integer])))::string).
  assert (_,tok(_,fltTok(64.0))) ^= nextToken(initSt(lc,"/*int*/64.0"::list[integer])).

  show(disp(nextToken(initSt(lc,"/*flt*/64.32"::list[integer])))::string).
  assert (_,tok(_,fltTok(64.32))) ^= nextToken(initSt(lc,"/*flt*/64.32"::list[integer])).

  show(disp(nextToken(initSt(lc,"/*flt*/64.32e-1"::list[integer])))::string).
  assert (_,tok(_,fltTok(6.432))) ^= nextToken(initSt(lc,"/*flt*/64.32e-1"::list[integer])).

  show disp(allTokens(initSt(lc,"foo bar()*45.3. "::list[integer])))::string.

  isTokens:(list[token],list[tk])=>boolean.
  isTokens([],[]) => true.
  isTokens([tok(_,Tk),..Toks],[Tk,..Tks]) => isTokens(Toks,Tks).
  isTokens(_,_) => false.

  assert isTokens(allTokens(initSt(lc,"foo bar()*45.3. "::list[integer])),
            [idTok("foo"),idTok("bar"),idTok("("), idTok(")"), idTok("*"), fltTok(45.3), idTok(". ")]).

  show disp(allTokens(initSt(lc,"\"alpha$(4+f(\"beta\"))$gamma\""::list[integer])))::string.
  assert isTokens(allTokens(initSt(lc,"\"alpha$(4+f(\"beta\"))$gamma\""::list[integer])),
            [strTok("alpha"), idTok("\$("), intTok(4), idTok("+"), idTok("f"),
             idTok("("), strTok("beta"),idTok(")"),idTok(")"),strTok("gamma")]).
}
