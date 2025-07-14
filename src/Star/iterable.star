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

  public all e ~~ generator[e] ~> fiber[res_generator,sus_generator[e]].

  public contract all c,e ~~ generate[c->>e] ::= {
    _generate:(c)=> generator[e]
  }

  public iterGenerator:all c,e ~~ iter[c->>e] |: (c) => generator[e].
  iterGenerator(L) => _fiber((this,first) => 
      let{
	yieldFn:(e,())=>().
	yieldFn(E,_) => valof{
	  case this suspend ._yld(E) in {
	  | ._next => {}
	  | ._cancel => retire ._all
	  };
	  valis ()
	}
      } in (case first in {
	| ._next => valof{
	    _iter(L,(),yieldFn);
	    valis ._all
	  }
	| ._cancel => ._all
      })).

  public implementation all e ~~ iter[generator[e]->>e] => {
    _iter:all x ~~ (generator[e],x,(e,x)=>x) => x.
    _iter(G,X,Fn) => valof{
      XX := X;
      case G resume ._next in {
	| ._yld(E) => { XX := Fn(E,XX!)}
	| ._all => valis XX!
      };
      valis XX!
    }
  }
}
