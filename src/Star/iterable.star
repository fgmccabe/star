star.iterable{
  import star.core.
  import star.action.

  -- The iter contract is used in query evaluation
  -- The _iter function iterates over the collection composing it

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x ~~ (s,x,(t,x)=>x) => x
  }

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }

  public contract all s,e ~~ iteration[s->>e] ::= {
    _current:(s) => option[e].
    _advance:(s) => result[(),()].
  }

  public contract all s,i ~~ iterator[s->>i] ::= {
    _iterator:(s) => i
  }
}
