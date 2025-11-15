test.jt3{
  import star.
  import star.assert.
  import test.lib.timer.

  conc:(string,string)=>string.
  conc(A,B) => _str_concat(A,B).

  main:(string,string)=>().
  main(A,B) => valof{
    showMsg(conc(A,B));

    try{
      _jit_compile("#(__pkg__)@conc",2);
    } catch {
      | .eNOPERM do showMsg("JIT not enabled")
      | Cde do showMsg("We got errr: $(Cde)")
    };

    showMsg(conc(A,B));
    
    assert conc("hello","there")=="hellothere";
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => main("hello ","world").
  _main([A,B]) => main(A,B).
}
    
