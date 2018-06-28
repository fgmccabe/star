star.fib{
  import star.

  -- Fibonacci priority queue

  fib[e] ::= q(e,integer,list[fib[e]]).

  root:all e ~~ order[e] |: (fib[e])=>e.
  root(q(x,_,_)) => x.

  rank:all e ~~  (fib[e])=>integer.
  rank(q(_,r,_)) => r.

  link:all e ~~ order[e] |: (q[e],q[e]) => q[e].
  link(q(x1,r,c1),q(x2,r,c2)) =>
    ( x2>=x1 ?
        q(x1,r+1,[q(x2,r,c2),..c1]) |
        q(x2,r+1,[q(x1,r,c1),..c2])).

  ins:all e ~~ order[e] |: (fib[e],list[fib[e]])=>list[fib[e]].
  ins(t,[]) => [t].
  ins(t,[t1,..ts]) where rank(t)<rank(t1) => [t,t1,..ts].
  ins(t,[t1,..ts]) => ins(link(t,t1),ts).

}
