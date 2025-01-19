star.monad{
  public contract all c/1 ~~ functor[c] ::= {
    fmap:all a,b ~~ ((a)=>b,c[a])=>c[b].
  }

  public contract all m/1 ~~ monad[m] ::= {
    (>>=) : all a,b ~~ (m[a],(a)=>m[b]) => m[b].
    (return): all a ~~ (a) => m[a].
  }

  public contract all e ~~ monoid[e] ::= {
    zed : e.
    (⊕):(e,e)=>e.
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
