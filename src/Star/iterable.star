star.iterable{
  import star.core.
  import star.monad.

  -- The iter contract is used in query evaluation
  -- The _iter function takes an execution and iterates over the collection composing it

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x,m/1,e ~~ execution[m->>e] |: (s,m[x],(t,x)=>m[x]) => m[x]
  }

  public all K,V ~~ keyval[K,V] ::= K->V.

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }
}
