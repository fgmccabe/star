rdf.graph{
  import star.
  import star.vector.

  import rdf.triple.

  public graph ::= graph{
    triples:vect[triple].
    subjects:map[concept,set[integer]].
    predicates:map[concept,set[integer]].
    objects:map[concept,set[integer]]}

  public nullGraph = graph{triples=nullV(). subjects=[]. predicates=[]. objects=[]}.

  public implementation display[graph] => {
    disp(G) => "{$(G.triples)}".
  }

  public addTriple:(graph,triple)=>graph.
  addTriple(G,Tr where .tr(S,P,O).=Tr) => valof{
    (G1,Sx) = resolveExistentials(G,S);
    (G2,Px) = resolveExistentials(G1,P);
    (G3,Ox) = resolveExistentials(G2,O);
    valis addTr(G3,Sx,Px,Ox)
  }

  resolveExistentials(G,.existential(AC,Trs)) =>
    (foldRight((Tr,Gi) => addTriple(Gi,Tr),G,Trs),AC).
  resolveExistentials(G,C) default => (G,C).

  addTr:(graph,concept,concept,concept)=>graph.
  addTr(G,S,P,O) => valof{
    Nx = size(G.triples);
    S1 = addToIndex(G.subjects,S,Nx);
    P1 = addToIndex(G.predicates,P,Nx);
    O1 = addToIndex(G.objects,O,Nx);
    valis graph{ count = Nx.
      triples = appnd(G.triples,.tr(S,P,O)).
      subjects = S1.
      predicates = P1.
      objects = O1
    }
  }

  addToIndex:(map[concept,set[integer]],concept,integer)=>map[concept,set[integer]].
  addToIndex(M,C,Ix) => valof{
    if isSymbolicConcept(C) then{
      if S ?= M[C] then
	valis M[C->S\+Ix]
      else
      valis M[C->[Ix]]
    } else
    valis M
  }

  validSubjects:(map[concept,set[integer]],vect[triple])=>boolean.
  validSubjects(S,Trs) => {? C->Ss in S *> (
      Ix in Ss *> ( .tr(Su,_,_) ?= Trs[Ix] && Su==C )) ?}.

  validPredicates:(map[concept,set[integer]],vect[triple])=>boolean.
  validPredicates(S,Trs) => {? C->Ss in S *> (
      Ix in Ss *> ( .tr(_,Pr,_) ?= Trs[Ix] && Pr==C )) ?}.

  validObjects:(map[concept,set[integer]],vect[triple])=>boolean.
  validObjects(S,Trs) => {? C->Ss in S *> (
      Ix in Ss *> ( .tr(_,_,Ob) ?= Trs[Ix] && Ob==C )) ?}.

  validTriples:(graph) => boolean.
  validTriples(G) => let{.
    presentInIndex:(concept,integer,map[concept,set[integer]])=>boolean.
    presentInIndex(C,Ix,Ind) => (isSymbolicConcept(C) ?? Ixs ?= Ind[C] && Ix .<. Ixs || .true).
  .} in ixRight((Ix,.tr(S,P,O),B) => B && presentInIndex(S,Ix,G.subjects) &&
	presentInIndex(P,Ix,G.predicates) &&presentInIndex(O,Ix,G.objects),.true,
    G.triples).

  public validGraph:(graph) => boolean.
  validGraph(G) =>
    validSubjects(G.subjects,G.triples) &&
	validPredicates(G.predicates,G.triples) &&
	    validObjects(G.objects,G.triples) &&
		validTriples(G).
}
