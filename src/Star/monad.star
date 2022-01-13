star.monad{
  public contract all c/1 ~~ functor[c] ::= {
    fmap:all a,b ~~ ((a)=>b,c[a])=>c[b].
    (<$):all a,b ~~ (a,c[b]) => c[a].
  }

  public contract all a/1 ~~ applicative[a] ::= {
    (pure): all x ~~ (x)=>a[x].
    (<*>):all x,y ~~ (a[(x)=>y],a[x])=>a[y].
  }

  public contract all m/1 ~~ monad[m] ::= {
    (>>=) : all a,b ~~ (m[a],(a)=>m[b]) => m[b].
    (return): all a ~~ (a) => m[a].
  }

  public contract all e ~~ monoid[e] ::= {
    zed : e.
    (⊕):(e,e)=>e.
  }

  public contract all m/2 ~~ execution[m] ::= {
    _perform:all a,e ~~ (m[e,a])=>a.
    _valis:all a,e ~~ (a)=>m[e,a].
    _sequence:all a,b,e ~~ (m[e,a],(a)=>m[e,b]) => m[e,b].
    _catch:all a,e,f ~~ (m[e,a],(e)=>m[f,a]) => m[f,a].
    _raise: all a,e ~~ (e) => m[e,a].
  }

  public contract all m/1,n/1 ~~ injection[m,n] ::= {
    _inject:all t ~~ (m[t])=>n[t].
  }

  -- public contract all cat/2 ~~ category[cat] ::= {
  --   id : all a ~~ cat[a,a].
  --   (•) : all a,b,c ~~ (cat[b,c],cat[a,b])=>cat[a,c].
  -- }

  public implementation monoid[integer] => {
    A ⊕ B => _int_plus(A,B).
    zed = 0.
  }
  
}
