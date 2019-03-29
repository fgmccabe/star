test.compose{
  import star.
  import star.combo.

  digit:(list[integer]) => option[(list[integer],integer)].
  digit([digitVal^(D),..L]) => some((L,D)).
  digit(_) => none.

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
  digitVal(_) => none.

  decimal:(list[integer]) => option[(list[integer],integer)].
  decimal(S) where digit(S)=.some((S1,D)) => moreDecimal(S1,D).
  decimal(_) => none.

  moreDecimal:(list[integer],integer) => option[(list[integer],integer)].
  moreDecimal(S,D) where digit(S)=.some((S1,D1)) =>
    moreDecimal(S1,D*10+D1).
  moreDecimal(S,D) => some((S,D)).

  assert decimal("123"::list[integer])==some(([],123)).

  dec:(list[integer]) => option[(list[integer],integer)].
  dec(S) where digit(S)=.some((S1,D)) => iter(S1,digit,(Dg,Nm)=>Nm*10+Dg,D).
  dec(_) => none.

  assert dec("1234"::list[integer])=.some(([],1234)).

  assert dec("123 "::list[integer])=.some(([0c ],123)).

  show "dec(123) = \(dec("123 "::list[integer]))"
}
