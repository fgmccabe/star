rdf.parser{
  import star.

  import star.location.

  import rdf.errors.
  import rdf.meta.
  import rdf.token.
  import rdf.triple.

  prefixDict ~> map[string,string].

  public parseGraph:() >> set[triple] --> cons[token].
  parseGraph >> Gr* --> preamble >> PrefixMap,
    rdfTriple(PrefixMap) * >> Gr, [.endTok(_)], {trP("grs $(Gr)")}.

  public rdfTriple:(prefixDict) >> set[triple] --> cons[token].
  rdfTriple(D) >> Ss --> triple(D) >> Ss, {trP("triple: $(Ss)")}.

  concept(D) >> C --> symbolic(D) >> C, {trP("Symbolic $(C)") }.
  concept(D) >> L --> literal(D) >> L, {trP("Literal $(L)") }.
  concept(D) >> .existential(Anon,A) -->
    [.tok(_,.pncTok("["))], { Anon.=genAnon() },
    verb_phrase(Anon,D) >> A, [.tok(_,.pncTok("]"))],
    {trP("Anonymous $(Anon), defining triples $(A)")}.
  
  symbolic(D) >> resolve(.named(Pre,Id),D) -->
    [.tok(_,.idTok(Pre)),.tok(_,.pncTok(":")),.tok(_,.idTok(Id))].
  symbolic(_) >> .named("",Id) -->
    [.tok(_,.idTok(Id))], ~[.tok(_,.pncTok(":"))].
  symbolic(_) >> .uri(U) --> [.tok(_,.uriTok(U))].

  literal(_) >> .int(Ix) --> [.tok(_,.intTok(Ix))].
  literal(_) >> .flt(Fx) --> [.tok(_,.fltTok(Fx))].
  literal(D) >> .text(Mkup) --> [.tok(Lc,.strTok(Sgs))], { Mkup ?= parseMarkup(Lc,Sgs,D) }.

  triple(D) >> S --> symbolic(D) >> Su, verb_phrase(Su,D) >> S,
  [.tok(_,.pncTok(". "))], {trP("triples $(S)")}.

  verb_phrase(Su,D) >> S* --> (symbolic(D) >> Pr, noun_phrase(Su,Pr,D))*[.tok(_,.pncTok(";"))] >> S.

  noun_phrase:(concept,concept,prefixDict) >> set[triple] --> cons[token].
  noun_phrase(Su,Pr,D) >> { .tr(Su,Pr,O) | O in Os } --> concept(D) * [.tok(_,.pncTok(","))] >> Os.

  parseMarkup(Lc,Sgs,D) =>
    ((Mkup,[]) ?= markups(Sgs,D) ??
      .some(Mkup) ||
      .none).

  markups(D) >> Ms --> markup(D)* >> Ms.

  markup(_) >> .str(Str) --> [.segment(_,Str)].
  markup(D) >> .link(C,S) --> [.interpolate(_,Tks,S)], {C ?= parseTks(concept,Tks,D)}.

  preamble:() >> prefixDict --> cons[token].
  preamble >> { P -> U | (P,U) in Ps } --> prefix * >> Ps.

  prefix :() >> (string,string) --> cons[token].
  prefix >> (P,U) -->
    [.tok(_,.pncTok("@")),.tok(_,.idTok("prefix")),
    .tok(_,.idTok(P)),.tok(_,.pncTok(":")),
    .tok(_,.uriTok(U)), .tok(_,.pncTok(". "))].

  parseTks:all t,r ~~ stream[t->>_] |= (((prefixDict)>>r-->t),t,prefixDict) => option[r].
  parseTks(P,T,D) => ( (R,[]) ?= P(T,D) ?? .some(R) || .none).

  resolve:(concept,prefixDict)=>concept.
  resolve(.named(Nm,Post),D) where Pr ?= D[Nm] => .uri(Pr++Post).
  resolve(C,_) default => C.

  trP(Msg) => valof{
    if traceParse! then{
      showMsg(Msg)
      };
    valis .true
  }
}
