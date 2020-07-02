test.rc{
  import star.
  import star.script.

  pp[a] ::= pp{C:integer} |
    pq{C:integer. A:a}.

  cont(C) => pp{.
    C=C
  .}

  main:()=>action[(),()].
  main()=>do{
    show "foo $(cont(2).C)"
  }
}
