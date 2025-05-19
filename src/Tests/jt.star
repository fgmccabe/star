test.jt{
  import star.
  import star.assert.

  inc42:(integer)=>integer.
  inc42(X) => _int_plus(X,42).

  main:()=>().
  main() => valof{
    assert inc42(0)==42;

    try{
      _jit_compile(inc42);
    } catch {
      Cde => showMsg("We got errr: $(Cde)")
    };

    assert inc42(3)==45;
    valis ()
  }
}
    
