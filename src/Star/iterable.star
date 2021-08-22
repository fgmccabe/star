star.iterable{
  import star.core.
  import star.monad.

  -- The iter contract is used in query evaluation
  -- The _iter function iterates over the collection composing it

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x ~~ (s,x,(t,x)=>x) => x
  }

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }

/*  public contract all s,t ~~ iteration[s->>t] ::= {
    _hasNext:(s)=>boolean.
    _current:(s) => t.
    _advance:(s) => result[(),()].
  }  */
}
