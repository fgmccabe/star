test.rc{
  import star.
  import star.script.

  pp[a] ::= pp{C:integer} |
    pq{C:integer. A:a}.

  cont(C) => pp{
    C=C
  }

  main:()=>action[(),()].
  main()=>action{
    show cont(2).C;
--    show cont(2).C<<-4;
--    show cont(2)<<-{C=4}
  }
}
