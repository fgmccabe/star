test.rc{
  import star.
  import star.script.

  pp ::= pp{
    C:integer.
  }

  cont(C) => pp{.
    C=C
  .}

  main:()=>action[(),()].
  main()=>do{
    show "foo $(cont(2).C)"
  }
}
