book.examples.lib.dijkstra{
  import star.
  import star.assert.

  /* Dijkstra's single-source shortest paths, purely functional.  A graph maps
   each node to its outgoing (neighbour, cost) edges.  Costs must be
   non-negative. The frontier is a pairing heap; stale entries are skipped when
   popped ("lazy deletion") instead of doing decrease-key, so nothing is ever
   updated in place.  */

  -- Pairing heap keyed by integer cost

  all e ~~ pq[e] ::= .empty | .node(integer,e,cons[pq[e]]).

  meld:all e ~~ (pq[e],pq[e]) => pq[e].
  meld(.empty,H) => H.
  meld(H,.empty) => H.
  meld(.node(K1,X1,C1),.node(K2,X2,C2)) =>
    (K1<K2 ?? .node(K1,X1,[.node(K2,X2,C2),..C1]) ||
      .node(K2,X2,[.node(K1,X1,C1),..C2])).

  push:all e ~~ (integer,e,pq[e]) => pq[e].
  push(K,X,H) => meld(.node(K,X,[]),H).

  mergePairs:all e ~~ (cons[pq[e]]) => pq[e].
  mergePairs([]) => .empty.
  mergePairs([H]) => H.
  mergePairs([H1,H2,..Hs]) => meld(meld(H1,H2),mergePairs(Hs)).

  pop:all e ~~ (pq[e]) => option[(integer,e,pq[e])].
  pop(.empty) => .none.
  pop(.node(K,X,Cs)) => .some((K,X,mergePairs(Cs))).

  -- Dijkstra proper
  -- Result maps every reachable node to (distance, predecessor).
  -- The source has predecessor .none; unreachable nodes are absent.

  public dijkstra:all n ~~ equality[n],hashable[n] |=
    (map[n,cons[(n,integer)]],n) => map[n,(integer,n)].
  dijkstra(G,Src) => let{.
    loop(Q,Done) => step(pop(Q),Done).

    step(.none,Done) => Done.
    step(.some((_,(U,_),Q)),Done) where _ ?= Done[U] => loop(Q,Done).
    step(.some((D,(U,P),Q)),Done) => loop(relax(U,D,Q),Done[U->(D,P)]).

    relax(U,D,Q) where Es ?= G[U] =>
      foldLeft(((V,W),Qx) => push(D+W,(V,U),Qx),Q,Es).
    relax(_,_,Q) default => Q.
  .} in loop(push(0,(Src,Src),.empty),[]).

  -- Walk predecessor links back from a target.

  public pathTo:all n ~~ equality[n],hashable[n] |=
    (map[n,(integer,n)],n) => option[(integer,cons[n])].
  pathTo(R,T) where (D,_) ?= R[T] => .some((D,path(R,T,[]))).
  pathTo(_,_) default => .none.

  path:all n ~~ equality[n],hashable[n] |=
    (map[n,(integer,n)],n,cons[n]) => cons[n].
  path(R,N,Acc) where (_,P) ?= R[N] && P~=N => path(R,P,[N,..Acc]).
  path(_,N,Acc) default => [N,..Acc].

  -- Example

  sampleGraph:map[string,cons[(string,integer)]].
  sampleGraph = {
    "a" -> [("b",7),("c",9),("f",14)],
    "b" -> [("a",7),("c",10),("d",15)],
    "c" -> [("a",9),("b",10),("d",11),("f",2)],
    "d" -> [("b",15),("c",11),("e",6)],
    "e" -> [("d",6),("f",9)],
    "f" -> [("a",14),("c",2),("e",9)]
  }.

  main:(){}.
  main(){
    R = dijkstra(sampleGraph,"a");
    show R;                  -- distances and predecessors
    show pathTo(R,"e");
    assert (20,["a","c","f","e"]) ?= pathTo(R,"e");
    show pathTo(R,"z");
    assert ~ _ ?= pathTo(R,"z");
  }
}
