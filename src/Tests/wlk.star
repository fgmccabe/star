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

  walker:all e ~~ (tree[e]) => generater[e].
  walker(T) => _fiber(
    (this,_) => valof{
      walk(T,(E,_) => valof{
	  yield E;
	  valis ()
	},());
      _retire(this,._all);
      valis ._all
    }).

  driver:(tree[string])=>().
  driver(T) => valof{
    W = walker(T);

    for E : W do{
      showMsg("next element in tree: #(E)")
    };
    showMsg("all elements done");
    valis ()
  }

  main:()=>().
  main() => valof{
    T = .node(.node(.empty,"A",.empty),"B",.node(.empty,"C",.node(.empty,"D",.empty)));

    show T;

    driver(T);

    valis ()
  }
}
    
