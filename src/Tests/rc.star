test.rc{
  import star.
  import star.assert.

  public pp[a] ::= pp{C:integer}.

  -- kk ::= kk{C:integer}.

  -- aa ::= aA{A:integer. ii:map[integer,boolean]. b:string. c:map[string,boolean]}.
  
  -- implementation measured[aa->>integer] => {
  --   [|A|] => [|A.ii|]
  -- }

  -- contract all a,b ~~ lc[a->>b] ::= {
  --   ll:(a)=>b
  -- }

  -- implementation all e ~~ lc[pp[e]->>integer] => {
  --   ll(pp{C=XX}) => XX.
  -- }

  -- implementation all a ~~ display[a] |: display[pp[a]] => {
  --   disp(pp{C=Ix}) => "pp{C=$(Ix)}".
  -- }

  public cont:(integer)=>pp[()].
  cont(C) => pp{
    C=C
  }

  public main:()=>().
  main()=>valof{
    show cont(2).C;
    -- AA = aA{A=10. ii=[1->.false, 2->.true]. b="hi". c=[]};

    -- show [|AA|];

    -- assert _?=AA.ii[2];
    -- assert ~_?=AA.ii[3];

    -- CC = (cont(2).C=20);
    -- show CC;
    -- show ll(CC);

    -- assert CC.C==20;

    valis ()
  }

}
