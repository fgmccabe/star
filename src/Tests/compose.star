test.compose{
  import star.

  seq:all s,c,x ~~ (option[(s,c)],((s,c))=>option[(s,x)]) => option[(s,x)].
  -- seq(O,F) => O>>=F.
  seq = (>>=).

  alt:all A,B ~~ ((A)=>option[B],(A)=>option[B]) => (A)=>option[B].
  alt(F1,F2) => let{
    aa:(A) => option[B].
    aa(X) where some(Y).=F1(X) => some(Y).
    aa(X) => F2(X).
  } in aa.

  digit:(list[integer]) => option[(list[integer],integer)].
  digit([D?.=digitVal,..L]) => some((L,D)).
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
  decimal(S) where some((S1,D)).=digit(S) => moreDecimal(S1,D).
  decimal(_) => none.

  moreDecimal:(list[integer],integer) => option[(list[integer],integer)].
  moreDecimal(S,D) where some((S1,D1)).=digit(S) =>
    moreDecimal(S1,D*10+D1).
  moreDecimal(S,D) => some((S,D)).

  assert decimal("123"::list[integer])==some(([],123)).

  /*
   disj:all s,c,x ~~ parse

  all x,s,c ~~ stream[s->>c] |: x-->s ~> (s) => option[(s,x)].

  -- Test simple grammars

  expr: (integer) --> list[integer].
  expr(S) => combine(factor(S),(SS)=>expr2()).

  expr2(S,X) => combine(terminal(0c+)(S),(SS)=>)
  expr(x+y) --> factor(x), [0c+], factor(y).
  expr(x-y) --> factor(x), "-", factor(y).
  expr(x) --> factor(x).

  factor:(integer) --> list[integer].
  factor(x*y) --> term(x), [0c*], term(y).
  factor(x) --> term(x).

  term:(integer) --> list[integer].

  term(x) --> [0c(], expr(x), [0c)].
  term(x) --> num(x).

term([0c(,..R]) => seq()

num:(integer) --> list[integer].
num([0c0,..R]) => some((R,0)).
num([0c1,..R]) => some((R,1)).
num(_) => none.

terminal:(integer)=>(() --> list[integer]).
terminal(X) => let{
  t:()-->list[integer].
  t([X,..R]) => some((R,())).
  t(_) => none.
} in t.

seq:((integer)-->list[integer],(integer)-->list[integer],
  (integer,integer)=>integer)=>(integer)-->list[integer].
seq(F,G,C) => let{
  t:(integer)-->list[integer].
  t(R) => check(F(R)).

  check:(option[(list[integer],integer)]) => option[(list[integer],integer)].
  check(some((R,X))) => check2(G(R),X).
  check(none) => none.

  check2:(option[(list[integer],integer)],integer) => option[(list[integer],integer)].
  check2(some((R,Y)),X) => some((R,C(X,Y))).
  check2(none) => none.
} in t.

    assert exp(X) .~ _explode("1+2*1") && X==3.
  /*
    assert foo(X) .= 3+3.

    assert some(([],X)) .= expr(_explode("1+2")) && X==3

    foo:(xxx)<=integer.

    expr : (integer) --> list[integer].
    expr(x+y) given (S) --> factor(x) given (S+1), [0c+], factor(y) given (S+x+2), +NT.
    expr(x-y) --> factor(x), "-", factor(y).
    expr(x) --> factor(x).
    */

}
