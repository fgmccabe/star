test.compose{
  import star.
  import star.combo.
  import star.assert.

  digit:(cons[char]) => option[(cons[char],integer)].
  digit([(Z where D?=digitVal(Z)),..L]) => .some((L,D)).
  digit(_) => .none.

  digitVal:(char)=>option[integer].
  digitVal(`0`) => .some(0).
  digitVal(`1`) => .some(1).
  digitVal(`2`) => .some(2).
  digitVal(`3`) => .some(3).
  digitVal(`4`) => .some(4).
  digitVal(`5`) => .some(5).
  digitVal(`6`) => .some(6).
  digitVal(`7`) => .some(7).
  digitVal(`8`) => .some(8).
  digitVal(`9`) => .some(9).
  digitVal(_) => .none.

  decimal:(cons[char]) => option[(cons[char],integer)].
  decimal(S) where (S1,D)?=digit(S) => moreDecimal(S1,D).
  decimal(_) => .none.

  moreDecimal:(cons[char],integer) => option[(cons[char],integer)].
  moreDecimal(S,D) where (S1,D1)?=digit(S) =>
    moreDecimal(S1,D*10+D1).
  moreDecimal(S,D) => .some((S,D)).


  dec:(cons[char]) => option[(cons[char],integer)].
  dec(S) where (S1,D)?=digit(S) => iter(S1,digit,(Dg,Nm)=>Nm*10+Dg,D).
  dec(_) => .none.

  main:()=>().
  main()=>valof{
    assert decimal("123"::cons[char])==.some(([],123));
    assert ([],1234)?=dec("1234"::cons[char]);

    assert ([` `],123)?=dec("123 "::cons[char]);

    show "dec(123) = $(dec("123 "::cons[char]))";
    valis ()
  }
}
