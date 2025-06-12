test.jt4{
  import star.
  import star.assert.

  conc:all e ~~ (cons[e],cons[e])=>cons[e].
  conc(.nil,Y) => Y.
  conc(.cons(E,X),Y) => .cons(E,conc(X,Y)).

  main:(integer)=>().
  main(C) => valof{
    L = (iota(1,C) : cons[integer]);
    M = iota(2,13);

    show conc(L,M);

    try{
      _jit_compile("#(__pkg__)@conc",2);
    } catch {
      X => showMsg("$(X)")
    };

    show conc(L,M);
    valis ()
  }
}
    
