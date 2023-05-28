star.iterable{
  import star.core.
  import star.fiber.

  -- The iter contract is used in query evaluation
  -- The _iter function iterates over the collection composing it

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x ~~ (s,x,(t,x)=>x) => x
  }

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }

  public sus_generator[e] ::= ._yld(e) | ._all.
  public res_generator ::= ._next | ._cancel.

  public contract all c,e ~~ generate[c->>e] ::= {
    _generate:(c)=>fiber[res_generator,sus_generator[e]]
  }

  public iterGenerator:all c,e ~~ iter[c->>e] |: (c) =>
    fiber[res_generator,sus_generator[e]].
  iterGenerator(L) => _new_fiber((this,first)=> valof{
      let{
	yieldFn:(e,())=>().
	yieldFn(E,_) => valof{
	  case _suspend(this,._yld(E)) in {
	    ._next => {}.
	    ._cancel => _retire(this,._all)
	  };
	  valis ()
	}
      } in {_ = _iter(L,(),yieldFn)};
      _retire(this,._all)}).
}
