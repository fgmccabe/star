test.l2{
  import star.

  tree[a] ::= empty | node(tree[a],a,tree[a]).

  implementation all e ~~ display[e] |: display[tree[e]] => let {
    dTree(empty) => ss("e").
    dTree(node(L,Lb,R)) => ssSeq([ss("<"),dTree(L),disp(Lb),dTree(R),ss(">")]).
  } in {.
    disp = dTree
  .}

  splits:(tree[(integer,string)]) => (tree[integer],list[string]).
  splits(T) => let{
    spl(empty) => (empty,[]).
    spl(node(L,(X,S),R)) => valof action{
      (LL,LX) .= spl(L);
      (RR,RX) .= spl(R);
      valis (node(LL,X,RR),LX++[S,..RX])
    }
  } in spl(T).

  show "splits $(splits(node(node(empty,(1,"a"),empty),(2,"b"),empty)))"
}
