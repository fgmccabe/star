rdf.sparql.solution{
  import star.
  import rdf.triple.

  /* Solution mappings and the core SPARQL algebra (Join/LeftJoin/Minus/Union/Filter/Extend),
     evaluated as multisets (plain lists, not sets -- SPARQL solution sequences are bag
     semantics, duplicates matter until DISTINCT/REDUCED) of mappings, per
     https://www.w3.org/TR/sparql12-query/#sparqlAlgebra (core algebra unchanged from
     SPARQL 1.1, which is what was cross-checked -- see the phased plan). Pure data/algebra:
     no dependency on the graph store or parser, so this is testable with hand-built mappings
     alone.

     Deliberately avoids {X | Y in L} comprehension syntax and cons[(k,v)]::map[k,v]
     coercion in plain function bodies -- both were found to hang star.compiler when used
     outside a grammar rule's output position (see the Phase 0 commit and the follow-up
     investigation task). Everything here uses plain recursive list matching plus // and ++
     instead, which are known to compile cleanly. */

  public mapping ~> map[string,concept].
  public emptyMapping:mapping. emptyMapping = [].

  public solutions ~> cons[mapping].
  public emptySolutions:solutions. emptySolutions = [].
  public unitSolutions:solutions. unitSolutions = [emptyMapping].

  -- Two mappings are compatible if they agree on every variable bound in both.
  public compatible:(mapping,mapping) => boolean.
  compatible(M1,M2) => ixRight((K,V1,Ok) => Ok && agrees(M2,K,V1), .true, M1).

  agrees(M2,K,V1) => (V2 ?= M2[K] ?? V1==V2 || .true).

  /* Assumes compatible(M1,M2) already holds; M2's bindings win on the overlap (necessarily
     identical values there, by compatibility), and everything from M1 not already in M2 is
     added. */
  public merge:(mapping,mapping) => mapping.
  merge(M1,M2) => ixRight((K,V,M) => M[K->V], M2, M1).

  public join:(solutions,solutions) => solutions.
  join([],_) => [].
  join([M1,..Rest],Om2) => joinOne(M1,Om2) ++ join(Rest,Om2).

  joinOne(_,[]) => [].
  joinOne(M1,[M2,..Rest]) where compatible(M1,M2) => [merge(M1,M2),..joinOne(M1,Rest)].
  joinOne(M1,[_,..Rest]) => joinOne(M1,Rest).

  public filterSol:((mapping)=>boolean,solutions) => solutions.
  filterSol(_,[]) => [].
  filterSol(F,[M,..Rest]) where F(M) => [M,..filterSol(F,Rest)].
  filterSol(F,[_,..Rest]) => filterSol(F,Rest).

  public leftJoin:(solutions,solutions,(mapping)=>boolean) => solutions.
  leftJoin(Om1,Om2,F) => filterSol(F,join(Om1,Om2)) ++ diffSol(Om1,Om2,F).

  /* Diff(Om1,Om2,F): mappings in Om1 with no compatible mapping in Om2 that also satisfies F
     once merged. Used only internally by leftJoin -- Diff has no surface SPARQL syntax of
     its own. */
  diffSol(Om1,Om2,F) => filterSol((M1) => ~hasCompatibleMatch(M1,Om2,F), Om1).

  hasCompatibleMatch(_,[],_) => .false.
  hasCompatibleMatch(M1,[M2,..Rest],F) =>
    (compatible(M1,M2) && F(merge(M1,M2))) || hasCompatibleMatch(M1,Rest,F).

  /* Minus(Om1,Om2): mappings in Om1 with no *domain-overlapping* compatible mapping in Om2
     -- a no-op if Om1/Om2 share zero variables (unlike leftJoin's Diff, which doesn't gate
     on domain overlap at all). This is deliberately different from NOT EXISTS, which
     re-evaluates per outer mapping regardless of shared variables -- see the plan's Risks
     section; engine.star's test suite covers the divergence explicitly. */
  -- Named minusSol, not minus, to avoid colliding with query.star's .minus pattern
  -- constructor -- the two are unrelated, but star.compiler doesn't disambiguate a plain
  -- function name from a same-named algebraic constructor cleanly, at least in engine.star's
  -- evalPattern(G,.minus(A,B)) clause head (confirmed: swap the name and it compiles fine).
  public minusSol:(solutions,solutions) => solutions.
  minusSol(Om1,Om2) => filterSol((M1) => ~hasOverlappingMatch(M1,Om2), Om1).

  hasOverlappingMatch(_,[]) => .false.
  hasOverlappingMatch(M1,[M2,..Rest]) =>
    (domainOverlaps(M1,M2) && compatible(M1,M2)) || hasOverlappingMatch(M1,Rest).

  domainOverlaps(M1,M2) => ixRight((K,_,Ov) => Ov || overlapsAt(M2,K), .false, M1).

  overlapsAt(M2,K) => (_ ?= M2[K] ?? .true || .false).

  public unionSol:(solutions,solutions) => solutions.
  unionSol(Om1,Om2) => Om1 ++ Om2.

  /* Assumes V is not already bound in M -- the caller (engine.star's BIND handling) is
     responsible for treating rebinding an already-bound variable as a SPARQL error. */
  public extend:(mapping,string,concept) => mapping.
  extend(M,V,C) => M[V->C].
}
