star.trie{
  import star.

  public trie[k,v] ::=
    .trNode(map[k,trie[k,v]],option[v]).

  public emptyTrie:all k,v ~~ equality[k],hashable[k] |: trie[k,v].
  emptyTrie = .trNode(_empty,.none).

  public findInTrie:all k,v ~~ equality[k],hashable[k] |:
    (cons[k],trie[k,v])=>option[v].
  findInTrie([],.trNode(_,Vl)) => Vl.
  findInTrie([K,..Ks],.trNode(M,_)) where Sub?=M[K] => findInTrie(Ks,Sub).
  findInTrie(_,_) default => .none.

  public insertInTrie:all k,v ~~ equality[k],hashable[k] |:
    (cons[k],v,trie[k,v]) => trie[k,v].
  insertInTrie([],Vl,.trNode(M,_)) => .trNode(M,.some(Vl)).
  insertInTrie([K,..Ks],V,.trNode(M,Vl)) where Sub ?= M[K] =>
    .trNode(M[K->insertInTrie(Ks,V,Sub)],Vl).
  insertInTrie([K,..Ks],V,.trNode(M,Vl)) =>
    .trNode(M[K->insertInTrie(Ks,V,.trNode([],.none))],Vl).

  public subTrie:all k,v ~~ equality[k],hashable[k] |: (trie[k,v]) => map[k,trie[k,v]].
  subTrie(.trNode(M,_)) => M.
}
  
