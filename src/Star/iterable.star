star.iterable{
  import star.core.
  import star.task.

  -- The iter contract is used in query evaluation
  -- The _iter function iterates over the collection composing it

  public all x ~~ _step[x] ::= _more(x) | _end(x).

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x ~~ (s,x,(t,x)=>_step[x]) => x
  }

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }

  public sus_generator[e] ::= _yld(e) | ._all.
  public res_generator ::= ._next | ._cancel.

  public contract all c,e ~~ generate[c->>e] ::= {
    _generate:(c)=>task[sus_generator[e],res_generator]
  }
}
