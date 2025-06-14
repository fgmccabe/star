test.jt6{
  import star.
  import star.assert.

  -- test jitting of closures and tail calls
  map:all e,f ~~ ((e)=>f,cons[e]) => cons[f].
  map(F,.nil) => .nil.
  map(F,.cons(E,X)) => .cons(F(E),map(F,X)).

  dbl:(integer)=>integer.
  dbl(X) => _int_plus(X,X).

  main:(integer)=>().
  main(C) => valof{
    L = (iota(1,C) : cons[integer]);

    show L;

    show map(dbl,L);

    try{
      _jit_compile("#(__pkg__)@map",2);
      _jit_compile("#(__pkg__)@dbl",1);
      _jit_compile("#(__pkg__)@dbl^",2);
    } catch {
      X => showMsg("$(X)")
    };

    show map(dbl,L);
    valis ()
  }
}  
