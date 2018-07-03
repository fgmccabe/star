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

  insert:all e ~~ order[e] |: (e,fib[e]) => fib[e].
  insert(x,t) => ins(q(x,0,[]),t).

  meld:all e~~order[e] |: (list[fib[e]],list[fib[e]]) => list[fib[e]].
  meld([],ts) => ts.
  meld(ts,[]) => ts.
  meld([t1,..ts1],[t2,..ts2]) where rank(t1)<rank(t2) => [t1,..meld(ts1,[t2,..ts2])].
  meld([t1,..ts1],[t2,..ts2]) where rank(t1)>rank(t2) => [t2,..meld([t1,..ts1],ts2)].
  meld([t1,..ts1],[t2,..ts2]) => ins(link(t1,t2), meld(ts1,ts2)).

  findMin:all e ~~ order[e]|:(list[fib[e]]) => e.
  findMin([t]) => root(t).
  findMin([t,..ts]) => let{
    x = findMin(ts).
  } in (x>=root(t) ? root(t) | x).

  deleteMin:all e ~~ order[e]|:(list[fib[e]]) => list[fib[e]].
  deleteMin(ts) => let{
    getMin([t])=>(t,[]).
    getMin([t,..tss]) where (t1,ts1) .= getMin(tss) => (
      root(t1)>=root(t1) ? (t,tss) | (t1,[t,..ts1])).
    (q(x,r,c),ts1) .= getMin(ts)
  } in meld(reverse(c),ts1).

}
