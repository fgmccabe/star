rdf.parser{
  import star.

  import star.location.

  import rdf.errors.
  import rdf.meta.
  import rdf.token.
  import rdf.triple.

  public rdfStmt:(cons[token]) => option[(set[stmt],cons[token])].
  rdfStmt >> {M} --> macro >> M.
  rdfStmt >> Ss --> sent >> Ss.

  macro >> .macro(Id,Rep) --> [.tok(_,.pncTok("@")),.tok(Lc,.idTok(Id)),.tok(_,.pncTok(":")),
    .tok(_,.strTok([.segment(_,Rep)])),.tok(_,.pncTok(". "))].

  concept >> .named(Pre,Id) --> [.tok(_,.idTok(Pre)),.tok(_,.pncTok(":")),.tok(_,.idTok(Id))].
  concept >> .named("",Id) --> [.tok(_,.idTok(Id))], ~[.tok(_,.pncTok(":"))].
  concept >> .uri(U) --> [.tok(_,.uriTok(U))].
  concept >> .int(Ix) --> [.tok(_,intTok(Ix))].
  concept >> .flt(Fx) --> [.tok(_.fltTok(Fx))].
  concept >> .text(Mkup) --> [.tok(Lc,.strTok(Sgs))], { Mkup ?= parseMarkup(Lc,Sgs) }.





}



