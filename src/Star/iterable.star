star.iterable{
  import star.core.

  -- The iter contract is used in query evaluation
  -- The _iter function iterates over the collection composing it

  public all x ~~ _step[x] ::= _more(x) | _end(x).

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x ~~ (s,x,(t,x)=>x) => x
  }

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }
}
