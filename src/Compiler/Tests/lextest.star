test.comp.lx{
  import star.
  import star.script.
  
  import star.compiler.errors.
  import star.compiler.lexer.
  import star.compiler.token.
  import star.compiler.location.
  import star.pkg.

  lc=pkgLoc(pkg("test",.defltVersion)).

  st:tokenState.
  st = initSt(lc,"  /* comment */\n-- comment\n `9` "::cons[char],reports([])).

  main:()=>action[(),()].
  main()=>action{
    show nextToken(initSt(lc,"\n`9`"::cons[char],reports([])));

    show nextToken(st);

    assert (_,some(tok(_,chrTok(`9`)))) .= nextToken(st);

    show nextToken(initSt(lc,"/*hex*/0x64"::cons[char],reports([])));

    assert (_,some(tok(_,intTok(100)))) .= nextToken(initSt(lc,"/*hex*/0x64"::cons[char],reports([])));

    show nextToken(initSt(lc,"/*int*/64"::cons[char],reports([])));
    assert (_,some(tok(_,intTok(64)))) .= nextToken(initSt(lc,"/*int*/64"::cons[char],reports([])));

    show nextToken(initSt(lc,"/*flt*/64.0"::cons[char],reports([])));
    assert (_,some(tok(_,fltTok(64.0)))) .= nextToken(initSt(lc,"/*int*/64.0"::cons[char],reports([])));

    show nextToken(initSt(lc,"/*flt*/64.32"::cons[char],reports([])));
    assert (_,some(tok(_,fltTok(64.32)))) .= nextToken(initSt(lc,"/*flt*/64.32"::cons[char],reports([])));

    show nextToken(initSt(lc,"/*flt*/64.32e-1"::cons[char],reports([])));
    assert (_,some(tok(_,fltTok(6.432)))) .= nextToken(initSt(lc,"/*flt*/64.32e-1"::cons[char],reports([])));

    show fst(allTokens(initSt(lc,"foo bar()*45. "::cons[char],reports([]))));

    assert isTokens(fst(allTokens(initSt(lc,"foo bar()*45. "::cons[char],reports([])))),
      [idTok("foo"),idTok("bar"),lftTok("()"), rgtTok("()"), idTok("*"), intTok(45), idTok(". ")]);

    show fst(allTokens(initSt(lc,"\"alpha\$(4+f(\"beta\"))gamma\""::cons[char],reports([]))));

    show allTokens(initSt(lc,"""{ disp(X)=> "$(X):c; pling #(X)" }"""::cons[char],reports([])));
  }

  isTokens:(cons[token],cons[tk])=>boolean.
  isTokens([],[]) => .true.
  isTokens([tok(_,Tk),..Toks],[Tk,..Tks]) => isTokens(Toks,Tks).
  isTokens(_,_) => .false.
}
