star.iterable{
  import star.core.
  import star.task.

  -- The iter contract is used in query evaluation
  -- The _iter function iterates over the collection composing it

  public contract all s,t ~~ iter[s->>t] ::= {
    _iter:all x ~~ (s,x,(t,x)=>x) => x
  }

  public contract all coll/1, m/2, k,v ~~ grouping[coll ->> m,k,v] ::=  {
    (group_by) : (coll[v], (v)=>k) => m[k,coll[v]]
  }

  public sus_generator[e] ::= _yld(e) | ._all.
  public res_generator ::= ._next | ._cancel.

  public contract all c,e ~~ generate[c->>e] ::= {
    _generate:(c)=>task[sus_generator[e],res_generator]
  }

  public iterGenerator:all c,e ~~ iter[c->>e] |: (c) => task[sus_generator[e],res_generator].
  iterGenerator(L) => task{
    let{
      yieldFn:(e,())=>().
      yieldFn(E,_) => valof{
	try{
	  suspend _yld(E) in {
	    ._next => {}.
	    ._cancel => throw ()
	  }
	} catch {
	  _ => retire ._all
	};
	valis ()
      }
    } in {_ .= _iter(L,(),yieldFn)};
    retire ._all
  }
  
}
