test.comp.gram{
  import star.
  import star.script.
  
  import star.compiler.ast.
  import star.compiler.errors.
  import star.compiler.opg.
  import star.compiler.parser.
  import star.compiler.token.
  import star.compiler.lexer.
  import star.compiler.location.
  import star.pkg.

  main:()=>action[(),()].
  main()=>action{
    lc.=pkgLoc(pkg("test",.defltVersion));

    show parseText(lc,"X. ",reports([]));

    assert (some(nme(_,"X")),_).=parseText(lc,"X. ",reports([]));

    assert (some(app(_,nme(_,"+"),tpl(_,"()",[int(_,2),int(_,3)]))),_) .=
      parseText(lc,"2+3. ",reports([]));

    show parseText(lc,"(X)=>X+Y*Z. ",reports([]));

    show parseText(lc,"f(X)(Y)=>X+Y*Z. ",reports([]));

    show parseText(lc,"{f(X)(Y)=>X+Y*Z}",reports([]));

    show parseText(lc,"{f(X)=>g(Y). g(Y)=>(X+Y)*Z}",reports([]));

    show parseText(lc,"{f(X)=>g(X.foo(Y)). g(Y)=>(X+Y)*Z}",reports([]));

    show parseText(lc,"{f(X,Y)=>g(X.foo(Y)). g(Y)=>(X+Y)*Z}",reports([]));

    show parseText(lc,"""{ dsp(X)=> "$(X):c;#(X)" }""",reports([]))
  }

}
