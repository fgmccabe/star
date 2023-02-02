test.cnt{
  import star.
  import star.script.

  add34() => prompt((T)=>34+control(T,(K)=>continue(K,8))).

  tree[a] ::= .empty | .node(tree[a],a,tree[a]).

  implementation all e ~~ display[e] |: display[tree[e]] => let {.
    dTree(.empty) => "e".
    dTree(.node(L,Lb,R)) => "<#(dTree(L))$(Lb)#(dTree(R))>".
  .} in {
    disp = dTree
  }

  main:() =>().
  main()=> valof{
    show add34();
    assert add34()==42;
    valis ()
  }
}
