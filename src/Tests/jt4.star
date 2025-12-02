test.jt4{
  import star.
  import star.assert.

  conc:all e ~~ (cons[e],cons[e])=>cons[e].
  conc(.nil,Y) => Y.
  conc(.cons(E,X),Y) => .cons(E,conc(X,Y)).

  main:(integer){}.
  main(C){
    L : cons[integer];
    L = iota(1,C);
    M = iota(2,13);

    show L;

    show conc(L,M);

    try{
      _jit_compile("#(__pkg__)@conc",2);
    } catch {
      | .eNOPERM do showMsg("JIT not enabled")
      | Cde do showMsg("We got errr: $(Cde)")
    };

    show conc(L,M);
  }

  _main:(cons[string]){}.
  _main([C,.._]) where Cnt?=(C:?integer) do main(Cnt).
  _main(_) do main(10).
}
    
