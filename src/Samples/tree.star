sample.tree{
  import star.

  public all e ~~ tree[e] ::= .eTree | .node(tree[e],e,tree[e]).

  public tVisit:all e,t ~~ (tree[e],(e,t)=>t,t) => t.
  tVisit(.eTree,_,A) => A.
  tVisit(.node(L,Lb,R),F,A) => tVisit(R,F,F(Lb,tVisit(L,F,A))).

  findInTree:all k,v ~~ equality[k],comp[k] |: (tree[(k,v)],k)=>option[v].
  findInTree(.eTree,_) => .none.
  findInTree(.node(L,(K,V),R),K) => .some(V).
  findInTree(.node(L,(K1,_),R),K) where K1>K => findInTree(L,K).

  insrt:all k,v ~~ equality[k],comp[k] |: (tree[(k,v)],k,v) => tree[(k,v)].
  insrt(.eTree,K,V) => .node(.eTree,(K,V),.eTree).
  insrt(.node(L,(K,_),R),K,V) => .node(L,(K,V),R).
  insrt(.node(L,(K1,V1),R),K,V) where K1<K => .node(L,(K1,V1),insrt(R,K,V)).
  insrt(.node(L,(K1,V1),R),K,V) where K1>K => .node(insrt(L,K,V),(K1,V1),R).

  -- This is a pretty slow algorithm
  mrge:all k,v ~~ equality[k],comp[v] |: (tree[(k,v)],tree[(k,v)])=>tree[(k,v)].
  mrge(.eTree,T) => T.
  mrge(T,.eTree) => T.
  mrge(.node(L1,(K1,V1),R1),T2) =>
    mrge(L1,mrge(R1,insrt(T2,(K1,V2)))).


}
