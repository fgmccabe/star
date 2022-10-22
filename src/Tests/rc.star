test.rc{
  import star.
  import star.script.

  public pp[a] ::= pp{C:integer} |
    pq{C:integer. A:a}.

  kk ::= kk{C:integer}.

  aa ::= aa{A:integer. ii:map[integer,boolean]}.
  

  implementation measured[aa->>integer] => {
    [|A|] => [|A.ii|]
  }

  contract all a,b ~~ lc[a->>b] ::= {
    ll:(a)=>b
  }

  implementation all e ~~ lc[pp[e]->>integer] => {
    ll(pp{C=XX}) => XX.
    ll(pq{C=YY}) => YY.
  }

  implementation all a ~~ display[a] |: display[pp[a]] => {
    disp(pp{C=Ix}) => "pp{C=$(Ix)}".
    disp(pq{C=Ix. A=A}) => "pq{C=$(Ix),A=$(A)}"
  }

  public cont:(integer)=>pp[()].
  cont(C) => pp{
    C=C
  }

  public main:()=>().
  main()=>valof{
    show cont(2).C;
    AA = aa{A=10. ii=[1->.false, 2->.true]};

    show [|AA|];

    assert ^AA.ii[2];
    assert ~_?=AA.ii[3];

    CC = (cont(2).C<<-20);
    show CC;
    show ll(CC);

    assert CC.C==20;

    valis ()
  }

}
