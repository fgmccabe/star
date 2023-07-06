star.finger{
  import star.core.
  import star.collection.
  import star.cons.
  import star.arith.
  import star.coerce.
  import star.monad.
  import star.iterable.
  import star.strings.
  import star.option.

  -- 2-3 finger trees.

  public fingerTree[a] ::=
    .eTree |
    .single(a) |
    .deep(integer,digit[a],fingerTree[node[a]],digit[a]).

  digit[a] ::= .one(a) | .two(a,a) | .three(a,a,a) | .four(a,a,a,a).

  node[a] ::= .node2(integer,a,a) | .node3(integer,a,a,a).

  contract all t,e ~~ reduce[t->>e] ::= {
    reducer:all a ~~ ((e,a)=>a) => (t,a) => a.
    reducel:all a ~~ ((e,a)=>a) => (t,a) => a.
  }

  implementation all e ~~ reduce[cons[e]->>e] => {
    reducer(F) => (L,U) => foldRight(F,U,L).
    reducel(F) => (L,U) => foldLeft(F,U,L).
  }

  implementation all e ~~ reduce[node[e]->>e] => {
    reducer = reducerNode.
    reducel = reducelNode.
  }
  
  reducerNode:all e,a ~~ ((e,a)=>a) => (node[e],a)=>a.
  reducerNode(F) => let{
    rdr(.node2(_,a,b),z) => F(a,F(b,z)).
    rdr(.node3(_,a,b,c),z) => F(a,F(b,F(c,z))).
  } in rdr.

  reducelNode:all e,a ~~ ((e,a)=>a) => (node[e],a)=>a.
  reducelNode(F) => let{
    rdl(.node2(_,b,a),z) => F(a,F(b,z)).
    rdl(.node3(_,c,b,a),z) => F(a,F(b,F(c,z))).
  } in rdl.

  implementation all e ~~ reduce[digit[e]->>e] => {
    reducer = reducerDigits.
    reducel = reducelDigits.
  }

  reducerDigits:all e,a ~~ ((e,a)=>a) => (digit[e],a)=>a.
  reducerDigits(F) => let{
    rdr(.one(x),z) => F(x,z).
    rdr(.two(u,v),z) => F(u,F(v,z)).
    rdr(.three(u,v,w),z) => F(u,F(v,F(w,z))).
    rdr(.four(u,v,w,x),z) => F(u,F(v,F(w,F(x,z)))).
  } in rdr.

  reducelDigits:all e,a ~~ ((e,a)=>a) => (digit[e],a)=>a.
  reducelDigits(F) => let{
    rdl(.one(u),z) => F(u,z).
    rdl(.two(u,v),z) => F(v,F(u,z)).
    rdl(.three(u,v,w),z) => F(w,F(v,F(u,z))).
    rdl(.four(u,v,w,x),z) => F(w,F(v,F(u,F(x,z)))).
  } in rdl.

  implementation all e ~~ coercion[digit[e],cons[e]] => {
    _coerce(.one(x)) => .some([x]).
    _coerce(.two(x,y)) => .some([x,y]).
    _coerce(.three(x,y,z)) => .some([x,y,z]).
    _coerce(.four(x,y,z,u)) => .some([x,y,z,u]).
  }

  implementation all e ~~ coercion[node[e],cons[e]] => {
    _coerce(.node2(_,l,r)) => .some([l,r]).
    _coerce(.node3(_,l,m,r)) => .some([l,m,r]).
  }
  
  implementation all e ~~ reduce[fingerTree[e]->>e] => {
    reducer = reducerTree.
    reducel = reducelTree.
  }

  reducerTree:all a,e ~~ ((e,a)=>a) => ((fingerTree[e],a)=>a).
  reducerTree(F) => let{.
    rdr(.eTree,z) => z.
    rdr(.single(u),z) => F(u,z).
    rdr(.deep(_,lft,mid,rgt),z) => let{
      F1 = reducerDigits(F).
      F2 = reducerTree(reducerNode(F)).
    } in F1(lft,F2(mid,F1(rgt,z)))
 .} in rdr.

  reducelTree:all e,a ~~ ((e,a)=>a) => (fingerTree[e],a)=>a.
  reducelTree(F) => let{.
    rdl(.eTree,z) => z.
    rdl(.single(u),z) => F(u,z).
    rdl(.deep(_,lft,mid,rgt),z) => let{
      F1 = reducelDigits(F).
      F2 = reducelTree(reducelNode(F)).
    } in F1(rgt,F2(mid,F1(lft,z))).
 .} in rdl.

  implementation all e ~~ concat[digit[e]] => {
  -- Partial, full implementation not possible.
    .one(a)++.one(b) => .two(a,b).
    .one(a)++.two(b,c) => .three(a,b,c).
    .one(a)++.three(b,c,d) => .four(a,b,c,d).
    .two(a,b)++.one(c) => .three(a,b,c).
    .two(a,b)++.two(c,d) => .four(a,b,c,d).
    .three(a,b,c)++.one(d) => .four(a,b,c,d).
    _multicat(.cons(T,.nil)) => T.
    _multicat(.cons(T,Ts)) => T++_multicat(Ts).
  }

  implementation all e ~~ head[digit[e]->>e] => {
    head(.one(x)) => .some(x).
    head(.two(x,_)) => .some(x).
    head(.three(x,_,_)) => .some(x).
    head(.four(x,_,_,_)) => .some(x).

    tail(.one(_)) => .none.
    tail(.two(_,x)) => .some(.one(x)).
    tail(.three(_,y,z)) => .some(.two(y,z)).
    tail(.four(_,x,y,z)) => .some(.three(x,y,z)).
  }

  prepend:all e ~~ measured[e->>integer] |: (e,fingerTree[e]) => fingerTree[e].
  prepend(a,.eTree) => .single(a).
  prepend(a,.single(b)) => .deep([|a|]⊕[|b|],.one(a),.eTree,.one(b)).
  prepend(a,.deep(M,.four(x,y,z,u),mid,r)) =>
    .deep([|a|]⊕M,.two(a,x),prepend(.node3([|y|]⊕[|z|]⊕[|u|],y,z,u),mid),r).
  prepend(a,.deep(M,l,mid,r)) => .deep([|a|]⊕M,.one(a)++l,mid,r).

  append:all e ~~ measured[e->>integer] |:
    (e,fingerTree[e]) => fingerTree[e].
  append(x,.eTree) => .single(x).
  append(x,.single(a)) => .deep([|x|]⊕[|a|],.one(a),.eTree,.one(x)).
  append(x,.deep(M,l,mid,.four(a,b,c,d))) =>
    .deep([|x|]⊕M,l,append(.node3([|a|]⊕[|b|]⊕[|c|],a,b,c),mid),.two(d,x)).
  append(x,.deep(M,l,mid,r)) => .deep([|x|]⊕M,l,mid,r++.one(x)).

  liftPrepend:all e,f ~~ reduce[f->>e],measured[e->>integer] |:
    (f,fingerTree[e]) => fingerTree[e].
  liftPrepend = reducer(prepend).

  liftAppend:all e,f ~~ reduce[f->>e],measured[e->>integer] |:
    (f,fingerTree[e])=>fingerTree[e].
  liftAppend = reducel(append).

  app3:all a ~~ measured[a->>integer] |:
    (fingerTree[a],cons[a],fingerTree[a]) => fingerTree[a].
  app3(.eTree,ts,xs) => liftPrepend(ts,xs).
  app3(xs,ts,.eTree) => liftAppend(ts,xs).
  app3(.single(x),ts,xs) => prepend(x,liftPrepend(ts,xs)).
  app3(xs,ts,.single(x)) => append(x,liftAppend(ts,xs)).
  app3(.deep(M1,pr1,m1,sf1),ts,.deep(M2,pr2,m2,sf2)) =>
    .deep(M1⊕M2,pr1,app3(m1,nodes(sf1::cons[a]++ts++pr2::cons[a]),m2),sf2).

  nodes:all e ~~ measured[e->>integer] |: (cons[e]) => cons[node[e]].
  nodes([a,b]) => [.node2([|a|]⊕[|b|],a,b)].
  nodes([a,b,c]) => [.node3([|a|]⊕[|b|]⊕[|c|],a,b,c)].
  nodes([a,b,c,d]) => [.node2([|a|]⊕[|b|],a,b),.node2([|c|]⊕[|d|],c,d)].
  nodes([a,b,c,..l]) => [.node3([|a|]⊕[|b|]⊕[|c|],a,b,c),..nodes(l)].

  public implementation all e ~~ measured[e->>integer] |:
    concat[fingerTree[e]] => {
      T1++T2 => app3(T1,[],T2).
      _multicat(.nil) => .eTree.
      _multicat(.cons(T,Ts)) => T++_multicat(Ts).
    }.

  public implementation all e ~~ measured[e->>integer] |:
    sequence[fingerTree[e]->>e] => {
      _cons(e,t) => prepend(e,t).
      _nil = .eTree.
    }.

  -- Implement meaasured contract
  implementation all a ~~ measured[node[a]->>integer] => {
    [|.node2(V,_,_)|]=>V.
    [|.node3(V,_,_,_)|] => V.
  }.

  implementation all a ~~ measured[a->>integer] |:
    measured[digit[a]->>integer] => {
      [|.one(A)|] => [|A|].
      [|.two(A,B)|] => [|A|]⊕[|B|].
      [|.three(A,B,C)|] => [|A|]⊕[|B|]⊕[|C|].
      [|.four(A,B,C,D)|] => [|A|]⊕[|B|]⊕[|C|]⊕[|D|].
    }.

  public implementation all a ~~ measured[a->>integer] |:
    measured[fingerTree[a]->>integer] => {
      [|.eTree|] => zed.
      [|.single(A)|] => [|A|].
      [|.deep(V,_,_,_)|] => V
    }.
  
  -- Implement iter contract

  public implementation all t ~~ measured[t->>integer] |:
    iter[fingerTree[t]->>t] => {.
      _iter:all x ~~ (fingerTree[t],x,(t,x)=>x) => x.
      _iter(Lst,St,Fn) => iterOverFinger(Lst,St,Fn).
    
      private iterOverFinger:all x ~~ (fingerTree[t],x,(t,x)=>x) => x.
      iterOverFinger(.eTree,St,_) => St.
      iterOverFinger(Tr,St,Fn) where 
	  .consl(El,tl) .= viewl(Tr) => iterOverFinger(tl,Fn(El,St),Fn).
    .}.

  public implementation all t ~~ measured[t->>integer] |: generate[fingerTree[t]->>t] => {
    _generate(T) => iterGenerator(T)
  }

  -- Implement display

  all s/1,a ~~ viewL[s,a] ::= .nill | .consl(a,s[a]).

  viewl:all e ~~ measured[e->>integer] |: (fingerTree[e])=>viewL[fingerTree,e].
  viewl(.eTree) => .nill.
  viewl(.single(E)) => .consl(E,.eTree).
  viewl(.deep(M,Lft,Md,Rgt)) where H?=head(Lft) =>
    .consl(H,deepL(tail(Lft),Md,Rgt)).

  toDigit:all e ~~ (node[e])=>digit[e].
  toDigit(.node2(_,x,y)) => .two(x,y).
  toDigit(.node3(_,x,y,z)) => .three(x,y,z).

  toTree:all e ~~ measured[e->>integer] |: (digit[e]) => fingerTree[e].
  toTree(.one(x)) => .single(x).
  toTree(.two(x,y)) => .deep([|x|]⊕[|y|],.one(x),.eTree,.one(y)).
  toTree(.three(x,y,z)) => .deep([|x|]⊕[|y|]⊕[|z|],.two(x,y),.eTree,.one(z)).
  toTree(.four(x,y,z,u)) => .deep([|x|]⊕[|y|]⊕[|z|]⊕[|u|],.two(x,y),.eTree,.two(z,u)).

  deepL:all e ~~ measured[e->>integer] |:
    (option[digit[e]],fingerTree[node[e]],digit[e]) => fingerTree[e].
  deepL(.none,Md,Rgt) =>
    ( .consl(H,M) .= viewl(Md) ??
	.deep([|H|]⊕[|M|]⊕[|Rgt|],toDigit(H),M,Rgt) ||
	toTree(Rgt)).
  deepL(.some(L),Md,Rgt) => .deep([|L|]⊕[|Md|]⊕[|Rgt|],L,Md,Rgt).

  public implementation all e ~~ measured[e->>integer] |:
    head[fingerTree[e]->>e] => {
      head(T) where .consl(h,_) .= viewl(T) => .some(h).
      head(_) => .none.

      tail(T) where .consl(_,t) .= viewl(T) => .some(t).
      tail(_) => .none.
    }.

  all s/1,a ~~ viewR[s,a] ::= .nilr | .consr(a,s[a]).

  viewr:all e ~~ measured[e->>integer] |: (fingerTree[e])=>viewR[fingerTree,e].
  viewr(.eTree) => .nilr.
  viewr(.single(E)) => .consr(E,.eTree).
  viewr(.deep(M,Lft,Md,Rgt)) where L?=last(Rgt) => .consr(L,deepR(lead(Rgt),Md,Lft)).

  deepR:all e ~~ measured[e->>integer] |:
    (option[digit[e]],fingerTree[node[e]],digit[e]) => fingerTree[e].
  deepR(.none,Md,Lft) =>
    ( .consr(L,F) .= viewr(Md) ??
	.deep([|Lft|]⊕[|F|]⊕[|L|],Lft,F,toDigit(L)) ||
	toTree(Lft)).
  deepR(.some(L),Md,Rgt) => .deep([|L|]⊕[|Md|]⊕[|Rgt|],L,Md,Rgt).

  implementation all e ~~ back[digit[e]->>e] => {
    last(.one(x)) => .some(x).
    last(.two(_,x)) => .some(x).
    last(.three(_,_,x)) => .some(x).
    last(.four(_,_,_,x)) => .some(x).

    lead(.one(_)) => .none.
    lead(.two(x,_)) => .some(.one(x)).
    lead(.three(x,y,_)) => .some(.two(x,y)).
    lead(.four(x,y,z,_)) => .some(.three(x,y,z)).
  }

  public implementation all e ~~ measured[e->>integer] |: back[fingerTree[e]->>e] => {
    last(T) where .consr(l,_) .= viewr(T) => .some(l).
    last(_) => .none.

    lead(T) where .consr(_,f) .= viewr(T) => .some(f).
    lead(_) => .none.
  }

  public implementation all e ~~ measured[e->>integer] |: stream[fingerTree[e]->>e] => {
    _eof(.eTree) => .true.
    _eof(_) => .false.

    _hdtl(T) where .consl(h,t) .= viewl(T) => .some((h,t)).
    _hdtl(_) => .none.
  }

  public implementation all e ~~ display[e] |: display[fingerTree[e]] => {
    disp(T) => "[#(interleave(dispTree(T,(x,L)=>.cons(disp(x),L),.nil),", ")*)]".
  }

  dispTree:all e ~~ (fingerTree[e],(e,cons[string])=>cons[string],cons[string]) => cons[string].
  dispTree(.eTree,_,L) => L.
  dispTree(.single(x),d,L) => d(x,L).
  dispTree(.deep(_,Lft,Md,Rgt),d,L) =>
    dispDigits(Lft,d,
      dispTree(Md,(N,LL)=>dispNode(N,d,LL),
	dispDigits(Rgt,d,L))).

  dispDigits:all e ~~ (digit[e],(e,cons[string])=>cons[string],cons[string]) => cons[string].
  dispDigits(.one(x),d,L) => d(x,L).
  dispDigits(.two(x,y),d,L) => d(x,d(y,L)).
  dispDigits(.three(x,y,z),d,L) => 
    d(x,d(y,d(z,L))).
  dispDigits(.four(x,y,z,u),d,L) => 
    d(x,
      d(y,
	d(z,
	  d(u,L)))).

  dispNode:all e ~~ (node[e],(e,cons[string])=>cons[string],cons[string]) => cons[string].
  dispNode(.node2(_,x,y),d,L) => d(x,d(y,L)).
  dispNode(.node3(_,x,y,z),d,L) => 
    d(x,d(y,d(z,L))).
  
}
