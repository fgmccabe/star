star.lex.nfa{
  import star.
  import star.compiler.errors.

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

  parseRe:(string)=>nfa[char].
  parseRe(Rs) => valof{
    (Re,Rest) = parseR(Rs::cons[char]);
    if ~isEmpty(Rest) then

  parseR(
    
}
