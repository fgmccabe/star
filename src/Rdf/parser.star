rdf.parser{
  import star.

  import star.location.

  import rdf.errors.
  import rdf.meta.
  import rdf.token.
  import rdf.triple.

  public parseGraph:() >> set[triple] --> cons[token].
  parseGraph >> Gr* --> rdfTriple * >> Gr, [.endTok(_)], {trP("grs $(Gr)")}.

  public rdfTriple:() >> set[triple] --> cons[token].
  rdfTriple >> Ss --> triple >> Ss.

  concept >> C --> symbolic >> C, {trP("Symbolic $(C)") }.
  concept >> L --> literal >> L, {trP("Literal $(L)") }.

  symbolic >> .named(Pre,Id) --> [.tok(_,.idTok(Pre)),.tok(_,.pncTok(":")),.tok(_,.idTok(Id))].
  symbolic >> .named("",Id) --> [.tok(_,.idTok(Id))], ~[.tok(_,.pncTok(":"))].
  symbolic >> .uri(U) --> [.tok(_,.uriTok(U))].
  
  literal >> .int(Ix) --> [.tok(_,.intTok(Ix))].
  literal >> .flt(Fx) --> [.tok(_,.fltTok(Fx))].
  literal >> .text(Mkup) --> [.tok(Lc,.strTok(Sgs))], { Mkup ?= parseMarkup(Lc,Sgs) }.

  triple >> S --> symbolic >> Su, verb_phrase(Su) >> S, [.tok(_,.pncTok(". "))], {trP("triples $(S)")}.

  verb_phrase(Su) >> S* --> (symbolic >> Pr, noun_phrase(Su,Pr))*[.tok(_,.pncTok(";"))] >> S.

  noun_phrase:(concept,concept) >> set[triple] --> cons[token].
  noun_phrase(Su,Pr) >> { .tr(Su,Pr,O) | O in Os } --> concept * [.tok(_,.pncTok(","))] >> Os.

  parseMarkup(Lc,Sgs) =>
    ((Mkup,[]) ?= markups(Sgs) ??
      .some(Mkup) ||
      .none).

  markups >> Ms --> markup* >> Ms.

  markup() >> .str(Str) --> [.segment(_,Str)].
  markup() >> .link(C,S) --> [.interpolate(_,Tks,S)], {C ?= parseTks(concept,Tks)}.

  parseTks:all t,r ~~ stream[t->>_] |: ((()>>r-->t),t) => option[r].
  parseTks(P,T) => ( (R,[]) ?= P(T) ?? .some(R) || .none).

  trP(Msg) => valof{
    if traceParse! then{
      showMsg(Msg)
      };
      valis .true
  }
}



