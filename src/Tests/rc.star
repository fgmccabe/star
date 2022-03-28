test.rc{
  import star.
  import star.script.

  pp[a] ::= pp{C:integer} |
    pq{C:integer. A:a}.

  kk ::= kk{C:integer}.

  aa ::= aa{A:integer}.

  implementation measured[aa->>integer] => {
    [|_|] => 1
  }

  contract all a,b ~~ lc[a->>b] ::= {
    ll:(a)=>b
  }

  implementation all a ~~ display[a] |: display[pp[a]] => {
    disp(pp{C=Ix}) => "pp{$(Ix)}".
    disp(pq{C=Ix. A=A}) => "pq{$(Ix),$(A)}"
  }

  cont:(integer)=>pp[()].
  cont(C) => pp{
    C=C
  }

  main:()=>action[(),()].
  main()=>action{
    show cont(2).C;
--    show cont(2)<<-{C=4}
  }

}
