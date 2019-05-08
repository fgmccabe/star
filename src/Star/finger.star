star.finger{
  import star.core.
  import star.collection.
  import star.cons.
  import star.arith.

  -- 2-3 finger trees.

  public all a ~~ fingerTree[a] ::=
    private eTree |
      private single(a) |
      deep(digit[a],fingerTree[node[a]],digit[a]).

  all a ~~ digit[a] ::= one(a) | two(a,a) | three(a,a,a) | four(a,a,a,a).

  all a ~~ node[a] ::= node2(a,a) | node3(a,a,a).

  implementation reduce[node] => {
    reducer(F) => let{
      rdr(node2(a,b),z) => F(a,F(b,z)).
      rdr(node3(a,b,c),z) => F(a,F(b,F(c,z))).
    } in rdr.

    reducel(F) => let{
      rdl(z,node2(b,a)) => F(F(z,b),a).
      rdl(z,node3(c,b,a)) => F(F(F(z,c),b),a).
    } in rdl.
  }

  implementation reduce[digit] => {
    reducer(F) => let{
      rdr(one(x),z) => F(x,z).
      rdr(two(u,v),z) => F(u,F(v,z)).
      rdr(three(u,v,w),z) => F(u,F(v,F(w,z))).
      rdr(four(u,v,w,x),z) => F(u,F(v,F(w,F(x,z)))).
    } in rdr.
    reducel(F) => let{
      rdl(z,one(u)) => F(z,u).
      rdl(z,two(u,v)) => F(F(z,u),v).
      rdl(z,three(u,v,w)) => F(F(F(z,u),v),w).
      rdl(z,four(u,v,w,x)) => F(F(F(F(z,x),u),v),w).
    } in rdl.
  }

  implementation all e ~~coercion[digit[e],list[e]] => {
    _coerce(one(x)) => [x].
    _coerce(two(x,y)) => [x,y].
    _coerce(three(x,y,z)) => [x,y,z].
    _coerce(four(x,y,z,u)) => [x,y,z,u].
  }

  implementation reduce[fingerTree] => {.
    reducer(F) => let{
      rdr(eTree,z) => z.
      rdr(single(u),z) => F(u,z).
      rdr(deep(lft,mid,rgt),z) => let{
        F1 = reducer(F).
        F2 = reducer(reducer(F)).
      } in F1(lft,F2(mid,F1(rgt,z)))
    } in rdr.
    reducel(F) => let{
      rdl(z,eTree) => z.
      rdl(z,single(u)) => F(z,u).
      rdl(z,deep(lft,mid,rgt)) => let{
        F1 = reducel(F).
        F2 = reducel(reducel(F)).
      } in F1(F2(F1(z,lft),mid),rgt).
    } in rdl.
  .}

  implementation all e ~~ concat[digit[e]] => {
  -- Partial, full implementation not possible.
    one(a)++one(b) => two(a,b).
    one(a)++two(b,c) => three(a,b,c).
    one(a)++three(b,c,d) => four(a,b,c,d).
    two(a,b)++one(c) => three(a,b,c).
    two(a,b)++two(c,d) => four(a,b,c,d).
    three(a,b,c)++one(d) => four(a,b,c,d).
  }

  prepend:all e ~~ (e,fingerTree[e]) => fingerTree[e].
  prepend(a,eTree) => single(a).
  prepend(a,single(b)) => deep(one(a),eTree,one(b)).
  prepend(a,deep(four(x,y,z,u),mid,r)) => deep(two(a,x),prepend(node3(y,z,u),mid),r).
  prepend(a,deep(l,mid,r)) => deep(one(a)++l,mid,r).

  append:all e ~~ (fingerTree[e],e) => fingerTree[e].
  append(eTree,x) => single(x).
  append(single(a),x) => deep(one(a),eTree,one(x)).
  append(deep(l,mid,four(a,b,c,d)),x) => deep(l,append(mid,node3(a,b,c)),two(d,x)).
  append(deep(l,mid,r),x) => deep(l,mid,r++one(x)).

  liftPrepend:all e,f/1 ~~ reduce[f] |: (f[e],fingerTree[e]) => fingerTree[e].
  liftPrepend = reducer(prepend).

  liftAppend:all e,f/1 ~~ reduce[f] |: (fingerTree[e],f[e])=>fingerTree[e].
  liftAppend = reducel(append).

  app3:all a ~~ (fingerTree[a],list[a],fingerTree[a]) => fingerTree[a].
  app3(eTree,ts,xs) => liftPrepend(ts,xs).
  app3(xs,ts,eTree) => liftAppend(xs,ts).
  app3(single(x),ts,xs) => prepend(x,liftPrepend(ts,xs)).
  app3(xs,ts,single(x)) => append(liftAppend(xs,ts),x).
  app3(deep(pr1,m1,sf1),ts,deep(pr2,m2,sf2)) =>
    deep(pr1,app3(m1,nodes(sf1::list[a]++ts++pr2::list[a]),m2),sf2).

  nodes:all e ~~ (list[e]) => list[node[e]].
  nodes([a,b]) => [node2(a,b)].
  nodes([a,b,c]) => [node3(a,b,c)].
  nodes([a,b,c,d]) => [node2(a,b),node2(c,d)].
  nodes([a,b,c,..l]) => [node3(a,b,c),..nodes(l)].

  implementation all e ~~ concat[fingerTree[e]] => {
    T1++T2 => app3(T1,[],T2).
  }
}
