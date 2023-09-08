rdf.parser{
  import star.

  import star.location.
  import star.parse.

  import rdf.errors.
  import rdf.meta.
  import rdf.token.
  import rdf.triple.

  punc:(string) => parser[cons[token],()].
  punc(P) => _sat(isPunc(P)) >>= (_) => return ().

  isPunc(P) => (.tok(_,Tk)) => .pncTok(P).=Tk.

  name:parser[cons[token],string].
  name = _test(isNameTok) >>= (Id) => return Id.

  isNameTok(.tok(_,.idTok(Nm))) => .some(Nm).
  isNameTok(_) default => .none.

  uriLit:parser[cons[token],string].
  uriLit = _test(isUriTok) >>= (Id) => return Id.

  isUriTok(.tok(_,.uriTok(U))) => .some(U).
  isUriTok(_) default => .none.

  macroDef:parser[cons[token],stmt].
  macroDef = punc("@") >>= (_) =>
    name >>= (Id) => punc(":") >>=
      (_) => uriLit >>=
	(U) => punc(". ") >>=
	  (_) => return .macro(Id,U).

  concept:parser[cons[token],concept].
  concept = namedConcept +++ literal.

  namedConcept:parser[cons[token],concept].
  namedConcept = (name >>= (Id) => return .named("",Id)) +++
  (name >>= (Pre) =>
      punc(":") >>= (_) =>
	name >>= (Suff) => return .named(Pre,Suff)).

  isStrTok(.tok(_,.strTok(S))) => .some(S).
  isStrTok(_) default => .none.

  literal:parser[cons[token],concept].
  literal = (uriLit >>= (U) => return .uri(U)) +++
  (_test(isStrTok) >>= (S) => return .text(S//)).

  sentence:parser[cons[token],stmt].
  sentence = concept >>= (S) =>
    concept >>= (P) =>
      concept >>= (O) =>
	punc(". ") >>= (_) => return .sent(.tr(S,P,O)).

  stmt:parser[cons[token],stmt].
  stmt = macroDef +++ sentence.


      

  
  -- parseMeta([.tok(Lc,.idTok("prefix")),..Tks],Map,Triples) => valof{
  --   if [.tok(PLc,.idTok(Nm)),.tok(_,.pncTok(":")),
  --     .tok(_,.uriTok(U)),.tok(_,.pncTok(". ")),..RTks] .=
  --     Tks then{
  -- 	valis (Map[Nm->U],Triples,RTks)
  --     } else{
  -- 	valis (Map,Triples,skipToEnd(Tks))
  --     }
  -- }
  

  -- public parseConcept:(cons[token],map[string,string]) =>
  --   (option[concept],cons[token]).
  -- parseConcept([.tok(Lc,Tk),..Tks],Map) => case Tk in {
  --   .idTok(S) => parseNamedConcept(Tks,Lc,S,Map)
  --   | .intTok(Ix) => (.some(.int(Ix)),Tks)
  --   | .fltTok(Dx) => (.some(.flt(Dx)),Tks)
  --   | .chrTok(Ch) => (.some(.text([.str(Ch::string)])),Tks)
  --   | .pncTok("@") => valof{
  --     if [.tok(_,.idTok("prefix")),..Tks1] .= Tks then
  -- 	valis (.some(.meta("prefix")),Tks1)
  --     else{
  -- 	reportError("unexpected meta operator",.some(Lc));
  -- 	valis (.none,Tks)
  --     }
  --   }
  --   | .pncTok(P) => valof{
  --     reportError("unexpected punctuation mark $(P)",.some(Lc));
  --     valis (.none,Tks)
  --   }
  --   | .uriTok(U) => (.some(.uri(U)),Tks)
  --   | .strTok(Segs) => (.some(.text(Segs//parseMarkup(Map))),Tks)
  --   | _ default => valof{
  --     reportError("cannot process token $(Tk), ignoring",.some(Lc));
  --     valis parseConcept(Tks,Map)
  --   }
  -- }.
  -- parseConcept([.endTok(Lc),.._],_) => (.none,[]).

  -- parseNamedConcept:(cons[token],locn,string,map[string,string])=>
  --   (option[concept],cons[token]).
  -- parseNamedConcept([.tok(Lcn,Tk),..Tks],Lc,Id,Mp) => valof{
  --   if [ .tok(_,.pncTok(":")),.tok(Lc1,.idTok(Suff)),..Tks1].=Tks then{
  --     if U ?= Mp[Id] then{
  -- 	valis (.some(.uri(U++Suff)),Tks1).
  --     } else{
  -- 	reportError("unknown prefix $(Id)",.some(Lc));
  -- 	valis (.none,Tks1)
  --     }
  --   } else
  --   valis (.some(.name(Id)),Tks)
  -- }

  -- parseMarkup(Map) => (Seg) => case Seg in {
  --   .segment(_Lc,Str) => .str(Str).
  --   .interpolate(ILc,Toks,Fmt) => valof{
  --     if (.some(Concept),Rst) .= parseConcept(Toks,Map) then {
  -- 	if [.tok(Lc,_),.._] .= Rst then
  -- 	  reportError("extra tokens in text markup",.some(Lc));
  -- 	valis .link(Concept,Fmt)
  --     } else{
  -- 	reportError("could not parse a concept in markup",.some(ILc));
  -- 	valis .str(Fmt)
  --     }
  --   }
  -- }

  -- public allConcepts:(cons[token],map[string,string]) => cons[concept].
  -- allConcepts([],_) => [].
  -- allConcepts([.endTok(_),.._],_) => [].
  -- allConcepts(Tks,Map) => valof{
  --   (Cpt,Rst) = parseConcept(Tks,Map);

  --   if traceParse! then
  --     logMsg("Concept $(Cpt)");
    
  --   Rem = allConcepts(Rst,Map);
  --   if Concept ?= Cpt then
  --     valis [Concept,..Rem]
  --   else
  --   valis Rem
  -- }
  
}



