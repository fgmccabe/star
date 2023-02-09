star.lex.nfa{
  import star.

  public nfa[c] ::=
    .epsilon |
    .choice(cons[nfa[c]]) |
    .one(c) |
    .star(nfa[c]).

  public implementation all c ~~ display[c] |: display[nfa[c]] => let{.
    dispNfa(.epsilon) => "ε".
    dispNfa(.choice(L)) => interleave(L//dispNfa,"|")*.
    dispNfa(.one(C)) => disp(C).
    dispNfa(.star(N)) => "#(dispNfa(N))*"
  .} in {
    disp(N) => dispNfa(N)
  }

  start:all c ~~ hashable[c], equality[c] |: (nfa[c]) => set[c].
  start(N) => let{.
    strt(.choice(L)) => 
    
}
