star.combo{
  import star.

  public seq:all s,c,x ~~ (option[(s,c)],((s,c))=>option[(s,x)]) => option[(s,x)].
  -- seq(O,F) => O>>=F.
  seq = (>>=).

  public alt:all A,B ~~ ((A)=>option[B],(A)=>option[B]) => (A)=>option[B].
  alt(F1,F2) => let{
      aa:(A) => option[B].
      aa(F1^(Y)) => some(Y).
      aa(X) => F2(X).
  } in aa.

  public iter:all A,B,C ~~ (A,(A)=>option[(A,B)],(B,C)=>C,C) => option[(A,C)].
  iter(S,St,F,Ix) where St(S)=.some((S1,D)) => iter(S1,St,F,F(D,Ix)).
  iter(S,_,_,Ix) => some((S,Ix)).

  public term:all c ~~ ((c)=>boolean)=>(list[c]) => option[(list[c],c)].
  term(P) => let{
      tt:(list[c])=>option[(list[c],c)].
      tt([C,..L]) where P(C) =>some((L,C)).
      tt(_) => .none.
  } in tt.

  public isK:all x ~~ equality[x] |: (x)=>((x) => boolean).
  isK(C) => ((Ch)=>Ch==C).
}
