star.monad{
  public contract all c/1 ~~ functor[c] ::= {
    fmap:all a,b ~~ ((a)=>b,c[a])=>c[b].
  }

  public contract all a/1 ~~ applicative[a] ::= {
    (pure): all x ~~ (x)=>a[x].
    (<*>):all x,y ~~ (a[(x)=>y],a[x])=>a[y].
  }

  public contract all m/1 ~~ monad[m] ::= {
    (>>=) : all a,b ~~ (m[a],(a)=>m[b]) => m[b].
    (return): all a ~~ (a) => m[a].
  }

  public contract all m/1 ~~ monadZero[m] ::= {
    zed : all x ~~ m[x].
  }

  public contract all m/1,e ~~ execution[m->>e] ::= {
    _perform:all a ~~ (m[a])=>a.
    _return:all a ~~ (a)=>m[a].
    _sequence:all a ~~ (m[a],(a)=>m[a]) => m[a].
    _handle:all a ~~ (m[a],(e)=>m[a]) => m[a].
    _raise: all a ~~ (e) => m[a].
  }

  public contract all m/1,n/1 ~~ injection[m,n] ::= {
    _inject:all t ~~ (m[t])=>n[t].
  }

  -- public contract all cat/2 ~~ category[cat] ::= {
  --   id : all a ~~ cat[a,a].
  --   (•) : all a,b,c ~~ (cat[b,c],cat[a,b])=>cat[a,c].
  -- }
}
