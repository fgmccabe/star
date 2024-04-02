star.iterable{
  import star.core.

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

  public all e ~~ generater[e] ~> res_generator=>>sus_generator[e].

  public contract all c,e ~~ generate[c->>e] ::= {
    _generate:(c)=> generater[e]
  }

  public iterGenerator:all c,e ~~ iter[c->>e] |: (c) => generater[e].
  iterGenerator(L) => (this spawn first =>> valof{
    let{
      yieldFn:(e,())=>().
      yieldFn(E,_) => valof{
	case this suspend ._yld(E) in {
	  ._next => {}.
	  ._cancel => this retire ._all
	};
	valis ()
      }
    } in {_ = _iter(L,(),yieldFn)};
    this retire ._all
    }).
}
