test.compose{
  import star.
  import star.combo.
  import star.script.

  digit:(cons[integer]) => option[(cons[integer],integer)].
  digit([digitVal^(D),..L]) => some((L,D)).
  digit(_) => .none.

  digitVal:(integer)=>option[integer].
  digitVal(0c0) => some(0).
  digitVal(0c1) => some(1).
  digitVal(0c2) => some(2).
  digitVal(0c3) => some(3).
  digitVal(0c4) => some(4).
  digitVal(0c5) => some(5).
  digitVal(0c6) => some(6).
  digitVal(0c7) => some(7).
  digitVal(0c8) => some(8).
  digitVal(0c9) => some(9).
  digitVal(_) => .none.

  decimal:(cons[integer]) => option[(cons[integer],integer)].
  decimal(S) where (S1,D)^=digit(S) => moreDecimal(S1,D).
  decimal(_) => .none.

  moreDecimal:(cons[integer],integer) => option[(cons[integer],integer)].
  moreDecimal(S,D) where (S1,D1)^=digit(S) =>
    moreDecimal(S1,D*10+D1).
  moreDecimal(S,D) => some((S,D)).


  dec:(cons[integer]) => option[(cons[integer],integer)].
  dec(S) where (S1,D)^=digit(S) => iter(S1,digit,(Dg,Nm)=>Nm*10+Dg,D).
  dec(_) => .none.

  main:()=>action[(),()].
  main()=>action{
    assert decimal("123"::cons[integer])==some(([],123));
    assert ([],1234)^=dec("1234"::cons[integer]);

    assert ([0c ],123)^=dec("123 "::cons[integer]);

    show "dec(123) = $(dec("123 "::cons[integer]))"
  }
}
