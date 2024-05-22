test.wlk{
  import star.
  import star.assert.

  -- Test recursive yield

  -- Tree type
  public tree[x] ::= .empty | .node(tree[x],x,tree[x]).

  implementation all e ~~ display[e] |: display[tree[e]] => let{.
    dispT(.empty) => "ε".
    dispT(.node(L,E,R)) => "<#(dispT(L)),$(E),#(dispT(R))>".
  .} in {
    disp = dispT
  }

  walk:all e,x ~~ (tree[e],(e,x)=>x,x) => x.
  walk(.empty,_,X) => X.
  walk(.node(L,E,R),F,X) => walk(R,F,F(E,walk(L,F,X))).

  main:()=>().
  main() => valof{
    T = .node(.node(.empty,"A",.empty),"B",.node(.empty,"C",.node(.empty,"D",.empty)));

    show T;
    valis ()
  }
}
    
