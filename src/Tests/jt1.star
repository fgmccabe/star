test.jt1{
  import star.
  import star.assert.

  fct:(integer)=>integer.
  fct(0)=>1.
  fct(N)=>_int_times(N,fct(_int_minus(N,1))).

  main:()=>().
  main() => valof{
    assert fct(3)==6;

    try{
      _jit_compile("test.jt1@fct",1);
    } catch {
      Cde => showMsg("We got errr: $(Cde)")
    };

    assert fct(4)==24;
    valis ()
  }
}
    
