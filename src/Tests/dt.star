test.dt{
  import star.
  import star.assert.

  tree[e] ::= .eT | nd(integer,tree[e],e,tree[e]).

  deleteTree:all v ~~ equality[v] |: (tree[v],integer,v)=>tree[v].
  deleteTree(.eT,_,_) => .eT.
  deleteTree(nd(Hash,L,Lb,R),Hash,_) => reformTree(Hash,L,R).
  deleteTree(nd(Hash,L,Lb,R),Hsh,K) => reformTree(ihLeaf(Hash,Els)).
  deleteTree(Sub,Dpth,Hash,K) where Ix.=subKey(Hash,Dpth) =>
    reformTree(patchVec(Sub,Ix,(Sb)=>deleteTree(Sb,Dpth+2,Hash,K))).

  removeMember:all k,v ~~ equality[k] |: (k,cons[keyval[k,v]])=>cons[keyval[k,v]].
  removeMember(K,cons(Ky->_,T)) where K==Ky => T.
  removeMember(K,cons(El,L)) => cons(El,removeMember(K,L)).
  removeMember(_,.nil) => .nil.
}
