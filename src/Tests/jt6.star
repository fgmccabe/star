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
    L = iota(1,C) |: cons[integer];

    show L;

    show map(dbl,L);

    try{
      _jit_compile("#(__pkg__)@map",2);
      _jit_compile("#(__pkg__)@dbl",1);
      _jit_compile("#(__pkg__)@dbl^",2);
    } catch {
      | .eNOPERM do showMsg("JIT not enabled")
      | Cde do showMsg("We got errr: $(Cde)")
    };

    show map(dbl,L);

    assert map(dbl,L) == [2,4,6,8,10,12,14,16,18];
    valis ()
  }

  _main:(cons[string]) => ().
  _main([C,.._]) where Cnt?=(C:?integer) => main(Cnt).
  _main(_) => main(10).
}  
