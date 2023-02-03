test.cnt{
  import star.
  import star.script.

  add34() => prompt((T)=>34+control(T,(K)=>continue(K,8))).

  tree[a] ::= .empty | .node(tree[a],a,tree[a]).

  step[a] ::= .done | .next(a,(())=>>step[a]).

  implementation all e ~~ display[e] |: display[tree[e]] => let {.
    dTree(.empty) => "e".
    dTree(.node(L,Lb,R)) => "<#(dTree(L))$(Lb)#(dTree(R))>".
  .} in {
    disp = dTree
  }

  walk:all a ~~ (t:tag[(),step[a]]) |: (tree[a]) => step[a].
  walk(.empty) => .done.
  walk(.node(L,Lb,R)) => valof{
    walk(L);
    yld(Lb);
    valis walk(R)
  }

  yld:all a ~~  (t:tag[(),step[a]]) |: (step[a])=>().
  

  main:() =>().
  main()=> valof{
    show add34();
    assert add34()==42;
    valis ()
  }
}
