test.rc{
  import star.
  import star.script.

  public pp[a] ::= pp{C:integer} |
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
    disp(pp{C=Ix}) => "pp{C=$(Ix)}".
    disp(pq{C=Ix. A=A}) => "pq{C=$(Ix),A=$(A)}"
  }

  public cont:(integer)=>pp[()].
  cont(C) => pp{
    C=C
  }

  public main:()=>action[(),()].
  main()=>action{
    show cont(2).C;
    show [|aa{A=10}|];

    CC .= (cont(2).C<<-20);
    show CC;

    assert CC.C==20
  }

}
