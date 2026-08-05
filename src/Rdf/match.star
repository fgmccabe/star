rdf.sparql.match{
  import star.
  import rdf.triple.
  import rdf.graph.
  import rdf.sparql.query.
  import rdf.sparql.solution.

  /* Indexed triple-pattern matching: evaluates a single .basic(S,P,O) pattern against a
     graph, producing the multiset of solution mappings binding this pattern's own
     variables. Each pattern is matched independently -- combining multiple patterns' results
     (BGP evaluation) is solution.star's join, applied by engine.star.

     v1 scope only: predicate must be .simple(T) (property paths -- .path(...) -- are v2,
     see the phased plan) and S/O must be .literal(concept)/.var (RDF-star pattern matching
     against .tripleTermPattern/.reifiedTriple terms is also v2, deferred pending a decision
     on reifier-identity semantics -- see query.star's annotationItem comment). Calling this
     with a .path predicate or those term shapes is a non-exhaustive-match error, same as
     how the rest of v1 draws this boundary.

     Per-position strategy: a bound *symbolic* term (.literal(C) with isSymbolicConcept(C))
     narrows via the matching graph index (subjects/predicates/objects); a bound but
     unindexed term (a literal value, e.g. .int/.flt/.text) or a repeated variable is instead
     checked by direct equality once a candidate triple is retrieved. If no position is
     bound-and-symbolic, this falls back to a full scan over G.triples -- graph.star has no
     composite (SPO/POS/etc.) index. */

  public matchBasic:(graph,term,predicate,term) => solutions.
  matchBasic(G,S,.simple(P),O) where Ixs ?= candidateSet(G,S,P,O) => matchIndices(G,Ixs,S,P,O).
  matchBasic(G,S,.simple(P),O) => matchFullScan(G,S,P,O).

  /* The intersection of every bound-and-symbolic position's index set, or .none if no
     position is bound-and-symbolic (signalling "fall back to a full scan"). Chains
     narrowStep directly over the three fixed positions rather than folding over a list
     literal -- passing an unannotated collection literal straight into a generic fold
     (foldRight/foldLeft/ixRight) was found to hang star.compiler, which can't resolve the
     literal's target collection type and the fold's polymorphic instance together; see the
     compiler-hang investigation task. */
  candidateSet:(graph,term,term,term) => option[set[integer]].
  candidateSet(G,S,P,O) =>
    narrowStep(indexFor(G.objects,O),narrowStep(indexFor(G.predicates,P),indexFor(G.subjects,S))).

  narrowStep:(option[set[integer]],option[set[integer]]) => option[set[integer]].
  narrowStep(.none,Acc) => Acc.
  narrowStep(Next,.none) => Next.
  narrowStep(.some(A),.some(B)) => .some(A/\B).

  emptyIxSet:set[integer]. emptyIxSet = [].

  indexFor:(map[concept,set[integer]],term) => option[set[integer]].
  indexFor(Ix,.literal(C)) where Sx ?= Ix[C] => .some(Sx).
  indexFor(_,.literal(C)) where isSymbolicConcept(C) => .some(emptyIxSet).
  indexFor(_,_) => .none.

  matchIndices:(graph,set[integer],term,term,term) => solutions.
  matchIndices(G,Ixs,S,P,O) => foldRight((Ix,Acc) => addMatch(G.triples[Ix],S,P,O,Acc),emptySolutions,Ixs).

  addMatch:(option[triple],term,term,term,solutions) => solutions.
  addMatch(.none,_,_,_,Acc) => Acc.
  addMatch(.some(Tr),S,P,O,Acc) where M ?= tryMatchTriple(Tr,S,P,O) => [M,..Acc].
  addMatch(.some(_),_,_,_,Acc) => Acc.

  matchFullScan:(graph,term,term,term) => solutions.
  matchFullScan(G,S,P,O) => foldRight((Tr,Acc) => addMatchDirect(Tr,S,P,O,Acc),emptySolutions,G.triples).

  addMatchDirect:(triple,term,term,term,solutions) => solutions.
  addMatchDirect(Tr,S,P,O,Acc) where M ?= tryMatchTriple(Tr,S,P,O) => [M,..Acc].
  addMatchDirect(_,_,_,_,Acc) => Acc.

  -- Binds S/P/O against Tr's own concepts, left to right, threading the growing mapping so
  -- a variable repeated across positions (e.g. ?s ex:p ?s) must agree on both occurrences.
  tryMatchTriple:(triple,term,term,term) => option[mapping].
  tryMatchTriple(.tr(Cs,Cp,Co),S,P,O) where M1 ?= matchTerm(emptyMapping,S,Cs) &&
      M2 ?= matchTerm(M1,P,Cp) => matchTerm(M2,O,Co).
  tryMatchTriple(_,_,_,_) => .none.

  matchTerm:(mapping,term,concept) => option[mapping].
  matchTerm(M,.literal(C),Actual) where C==Actual => .some(M).
  matchTerm(_,.literal(_),_) => .none.
  matchTerm(M,.var(V),Actual) where Bound ?= M[V] => (Bound==Actual ?? .some(M) || .none).
  matchTerm(M,.var(V),Actual) => .some(M[V->Actual]).
}
