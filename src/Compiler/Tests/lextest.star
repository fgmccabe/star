test.comp.lx{
  import star.
  import star.script.
  
  import star.compiler.lexer.
  import star.compiler.token.
  import star.compiler.location.
  import star.pkg.

  lc=pkgLoc(pkg("test",defltVersion)).

  st:tokenState.
  st = initSt(lc,"  /* comment */\n-- comment\n 0c9 "::cons[integer]).

  main:()=>action[(),()].
  main()=>action{
    show nextToken(initSt(lc,"\n0c9"::cons[integer]));

    show nextToken(st);

    assert (_,tok(_,intTok(57))) ^= nextToken(st);

    show nextToken(initSt(lc,"/*hex*/0x64"::cons[integer]));

    assert (_,tok(_,intTok(100))) ^= nextToken(initSt(lc,"/*hex*/0x64"::cons[integer]));

    show nextToken(initSt(lc,"/*int*/64"::cons[integer]));
    assert (_,tok(_,intTok(64))) ^= nextToken(initSt(lc,"/*int*/64"::cons[integer]));

    show nextToken(initSt(lc,"/*flt*/64.0"::cons[integer]));
    assert (_,tok(_,fltTok(64.0))) ^= nextToken(initSt(lc,"/*int*/64.0"::cons[integer]));

    show nextToken(initSt(lc,"/*flt*/64.32"::cons[integer]));
    assert (_,tok(_,fltTok(64.32))) ^= nextToken(initSt(lc,"/*flt*/64.32"::cons[integer]));

    show nextToken(initSt(lc,"/*flt*/64.32e-1"::cons[integer]));
    assert (_,tok(_,fltTok(6.432))) ^= nextToken(initSt(lc,"/*flt*/64.32e-1"::cons[integer]));

    show allTokens(initSt(lc,"foo bar()*45. "::cons[integer]));

    assert isTokens(allTokens(initSt(lc,"foo bar()*45. "::cons[integer])),
      [idTok("foo"),idTok("bar"),lftTok("()"), rgtTok("()"), idTok("*"), intTok(45), idTok(". ")]);

    show allTokens(initSt(lc,"\"alpha\$(4+f(\"beta\"))gamma\""::cons[integer]));
  }

  isTokens:(cons[token],cons[tk])=>boolean.
  isTokens([],[]) => true.
  isTokens([tok(_,Tk),..Toks],[Tk,..Tks]) => isTokens(Toks,Tks).
  isTokens(_,_) => false.
}
