star.heap{
  import star.

  -- implementation of a binomial heap, otherwise known as a priority queue
  -- From Okasaki's book

  public all x ~~ tree[x] ::= .eNode(x,integer,cons[tree[x]]).
  public all x ~~ heap[x] ::= .h(cons[tree[x]]).

  rank:all x ~~ (tree[x])=>integer.
  rank(.eNode(_,K,_)) => K.

  root:all x ~~ (tree[x]) => x.
  root(.eNode(X,_,_)) => X.

  link:all x ~~ (leq:(x,x)=>boolean) |: (tree[x],tree[x]) => tree[x].
  link(.eNode(X1,K,C1),.eNode(X2,_,C2)) =>
    (leq(X1,X2) ?? .eNode(X1,K+1,[.eNode(X2,K,C2),..C1]) ||
      .eNode(X2,K+1,[.eNode(X1,K,C1),..C2])).

  insTree:all x ~~ (leq:(x,x)=>boolean) |: (tree[x],cons[tree[x]]) => cons[tree[x]].
  insTree(t,[]) => [t].
  insTree(t,[f,..ts]) => (rank(t)<rank(f) ?? [t,f,..ts] || insTree(link(t,f),ts)).

  insert:all x ~~ (leq:(x,x)=>boolean) |: (x,cons[tree[x]]) => cons[tree[x]].
  insert(x,ts) => insTree(.eNode(x,0,[]),ts).

  merge:all x ~~ (leq:(x,x)=>boolean) |: (cons[tree[x]],cons[tree[x]]) => cons[tree[x]].
  merge(T,[]) => T.
  merge([],T) => T.
  merge([T1,..Ts1],[T2,..Ts2]) => valof{
    if rank(T1)<rank(T2) then
      valis [T1,..merge(Ts1,[T2,..Ts2])]
    else if rank(T1)>rank(T2) then
      valis [T2,..merge([T1,..Ts1],Ts2)]
    else
    valis insTree(link(T1,T2),merge(Ts1,Ts2))
  }

  removeMinTree:all x ~~ (leq:(x,x)=>boolean) |: (cons[tree[x]]) => (tree[x],cons[tree[x]]).
  removeMinTree([T]) => (T,[]).
  removeMinTree([T,..Ts]) => valof{
    (T1,Ts1) = removeMinTree(Ts);
    if leq(root(T),root(T1)) then
      valis (T,Ts)
    else
    valis (T1,[T,..Ts1])
  }

  findMin:all x ~~ (leq:(x,x)=>boolean) |: (heap[x]) => option[x].
  findMin(.h([])) => .none.
  findMin(.h(Ts)) => valof{
    (T,_) = removeMinTree(Ts);
    valis .some(root(T))
  }

  deleteMin:all x ~~ (leq:(x,x)=>boolean) |: (heap[x]) => heap[x].
  deleteMin(.h(Ts)) => valof{
    (.eNode(_,_,Ts1),Ts2) = removeMinTree(Ts);
    valis .h(merge(reverse(Ts1),Ts2))
  }

  public showHeap: all x ~~ display[x] |: (heap[x])=>string.
  showHeap(.h(Hs)) => let{.
    sh(H) => interleave(H//shT,", ")*.

    shT(.eNode(X,_,C)) => "$(X) [#(sh(C))]".
  .} in "[#(sh(Hs))]".

  -- Implement some contracts

  public implementation all x ~~ display[x] |: display[heap[x]] => let{.
    sh(H) => (H//shT)*.

    shT(.eNode(X,_,C)) => [disp(X),..sh(C)].
  .} in {
    disp(.h(H)) => "[#(interleave(sh(H),", ")*)]".
  }

  public implementation all x ~~ (leq:(x,x)=>boolean) |: sequence[heap[x]->>x] => {
    _nil = .h([]).
    _cons(E,.h(C)) => .h(insert(E,C))
  }

  public implementation all x ~~ (leq:(x,x)=>boolean) |: stream[heap[x]->>x] => {
    _eof(.h(.nil)) => .true.
    _eof(_) default => .false.

    _hdtl(.h(.nil)) => .none.
    _hdtl(.h(Ts)) => valof{
      (.eNode(R,_,Ts1),Ts2) = removeMinTree(Ts);
      valis .some((R,.h(merge(reverse(Ts1),Ts2))))
    }
  }

  walk:all x,s ~~ (leq:(x,x)=>boolean) |: (heap[x],(x,s)=>s,s) => s.
  walk(.h(Ts),F,Sinit) => let{.
    w([],St) => St.
    w(Cs,St) => valof{
      (.eNode(X,_,Rs),Ts2) = removeMinTree(Cs);
      valis w(merge(reverse(Rs),Ts2),F(X,St))
    }
  .} in w(Ts,Sinit).

  public implementation all x ~~ (leq:(x,x)=>boolean) |: iter[heap[x] ->> x] => {
    _iter(H,Z,F) => walk(H,F,Z)
  }

  public implementation all x ~~ (leq:(x,x)=>boolean) |: generate[heap[x]->>x] => {
    _generate(Ts) => generator{
      TT := Ts;
      while [H,..T] .= TT! do{
	yield H;
	TT := T
      }
    }
  }

  public implementation all x ~~ (leq:(x,x)=>boolean) |: concat[heap[x]] => {
    .h(L) ++ .h(R) => .h(merge(L,R)).
    _multicat(Hs) => let{.
      m([]) => [].
      m([.h(T),..Ts]) => merge(T,m(Ts)).
    .} in .h(m(Hs))
  }
}
