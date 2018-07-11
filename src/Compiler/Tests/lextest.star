test.comp.lx{
  import star.
  import star.compiler.lexer.
  import star.compiler.token.
  import star.compiler.location.
  import star.pkg.

  p:pkg.
  p=pkg("test",defltVersion).

  st:tokenState.
  st = initSt(p,"  /* comment */\n-- comment\n 0c9 "::list[integer]).

  show(disp(nextToken(initSt(p,"\n0c9"::list[integer])))::string).

  show(disp(nextToken(st))::string).

  assert (_,tok(_,intTok(57))) ^= nextToken(st).

  show(disp(nextToken(initSt(p,"/*hex*/0x64"::list[integer])))::string).

  assert (_,tok(_,intTok(100))) ^= nextToken(initSt(p,"/*hex*/0x64"::list[integer])).

  show(disp(nextToken(initSt(p,"/*int*/64"::list[integer])))::string).
  assert (_,tok(_,intTok(64))) ^= nextToken(initSt(p,"/*int*/64"::list[integer])).

  show(disp(nextToken(initSt(p,"/*flt*/64.0"::list[integer])))::string).
  assert (_,tok(_,fltTok(64.0))) ^= nextToken(initSt(p,"/*int*/64.0"::list[integer])).

  show(disp(nextToken(initSt(p,"/*flt*/64.32"::list[integer])))::string).
  assert (_,tok(_,fltTok(64.32))) ^= nextToken(initSt(p,"/*flt*/64.32"::list[integer])).

  show(disp(nextToken(initSt(p,"/*flt*/64.32e-1"::list[integer])))::string).
  assert (_,tok(_,fltTok(6.432))) ^= nextToken(initSt(p,"/*flt*/64.32e-1"::list[integer])).

  show disp(allTokens(initSt(p,"foo bar()*45.3. "::list[integer])))::string.

  isTokens:(list[token],list[tk])=>boolean.
  isTokens([],[]) => true.
  isTokens([tok(_,Tk),..Toks],[Tk,..Tks]) => isTokens(Toks,Tks).

  assert isTokens(allTokens(initSt(p,"foo bar()*45.3. "::list[integer])),
            [idTok("foo"),idTok("bar"),idTok("("), idTok(")"), idTok("*"), fltTok(45.3), idTok(". ")]).

}
