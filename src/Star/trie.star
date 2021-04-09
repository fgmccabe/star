trie.star{
  import star.

  public trie[k,v] ::=
    private trNode(map[k,trie[k,v]],option[v]).

  public findInTrie:all k,v ~~ equality[k],hash[k] |:
    (cons[k],trie[k,v])=>option[v].
  findInTrie([],trNode(_,Vl)) => Vl.
  findInTrie([K,..Ks],trNode(M,_)) where Sub^=M[K] => findInTrie(Ks,Sub).
  findInTrie(_,_) default => .none.

  findRest([],_,Vl) => Vl.
  findRest(Ks,Sub,_) => findInTrie(Ks,Sub).

  public insertInTrie:all k,v ~~ equality[k],hash[k] |:
    (cons[k],v,trie[k,v]) => trie[k,v].
  insertInTrie([],Vl,trNode(M,_) => trNode(M,some(Vl)).
  insertInTrie([K,..Ks],V,trNode(M,Vl)) where Sub ^= M[K] =>
    trNode(M[K->insertInTrie(Ks,V,Sub)],Vl).
  insertInTrie([K,..Ks],V,trNode(M,Vl)) =>
    trNode(M[K->insertInTrie(Ks,trNode({},.none))],Vl).

  

  
}
  
