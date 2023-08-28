rdf.parser{
  import star.

  import star.location.

  import rdf.errors.
  import rdf.token.
  import rdf.triple.

  public parseConcept:(cons[token],map[string,token]) =>
    (option[concept],cons[token]).
  parseConcept([.tok(Lc,Tk),..Tks],Map) => case Tk in {
    .idQTok(S) => parseNamedConcept(Tks,Lc,S,[],Map)
    | .idTok(S) => parseNamedConcept(Tks,Lc,S,[],Map)
    | .intTok(Ix) => (.int(Ix),Tks)
    | .fltTok(Dx) => (.flt(Dx),Tks)
    | .chrTok(Ch) => (.text(Ch::string),Tks)
    | .pncTok(string)
    | .uriTok(U) => (.uri(U),Tks)
    | .strTok([.segment(Txt)]) => (.text(Txt),Tks)
    | _ default => valof{
      reportError("cannot process token $(Tk), ignoring",.some(Lc));
      valis parseConcept(Tks,Map)
    }
  }.
  parseConcept([.endTok(Lc),.._],_) => (.none,[]).

  parseNameConcept([.tok(Lc,Tk),..Tks],Lc,P,SoFar,Mp) => case Tk in {
    .punc(":") => valof{
      if Entry ?= Mp[P] then{
	case Entry in {
	  .uri(U)
  }
}



