star.monad{
  public contract all m/1 ~~ monad[m] ::= {
    (>>=) : all a,b ~~ (m[a],(a)=>m[b]) => m[b].
    (return): all a ~~ (a) => m[a].
  }

  public contract all m/1 ~~ monadZero[m] ::= {
    zed : all x ~~ m[x].
  }

  public contract all m/1,e ~~ execution[m->>e] ::= {
    _perform:all a ~~ (m[a])=>a.
    _handle:all a ~~ (m[a],(e)=>m[a]) => m[a].
    _raise: all a ~~ (e) => m[a].
  }

  public contract all m/1,n/1 ~~ injection[m,n] ::= {
    _inject:all t ~~ (m[t])=>n[t].
  }
}
