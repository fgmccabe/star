test.l2{
  import star.
  import star.script.

  tree[a] ::= .empty | node(tree[a],a,tree[a]).

  implementation all e ~~ display[e] |: display[tree[e]] => let {.
    dTree(.empty) => "e".
    dTree(node(L,Lb,R)) => "<#(dTree(L))$(Lb)#(dTree(R))>".
  .} in {
    disp = dTree
  }

  splits:(tree[(integer,string)]) => (tree[integer],cons[string]).
  splits(T) => let{.
    spl(.empty) => (.empty,[]).
    spl(node(L,(X,S),R)) => valof action{
      (LL,LX) .= spl(L);
      (RR,RX) .= spl(R);
      valis (node(LL,X,RR),LX++[S,..RX])
    }
  .} in spl(T).

  main:()=>action[(),()].
  main()=> action{
    show "splits $(splits(node(node(.empty,(1,"a"),.empty),(2,"b"),.empty)))"
  }
}
