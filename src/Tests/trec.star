test.trec{
  import star.
  import star.assert.

  codeCtx ::= codeCtx{
    lbls : ref integer.  
    min : integer.
    hwm : ref integer.
    brks : map[string,Cont]
  }.

  Cont ::= cont{
    C:(codeCtx,stack)=>stack.
  }.

  stack ~> option[cons[integer]].

  main:()=>().
  main()=>valof{
    logMsg("Just for show");
    valis ()
  }
}


