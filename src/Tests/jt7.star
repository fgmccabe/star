test.jt7{
  import star.
  import star.assert.

  -- Test jitting of global variables

  conc:(string,string)=>string.
  conc(A,B) => _str_concat(A,B).

  triggered = ref .false.

  glb:string.
  glb = valof{
    _logmsg("glb triggered");
    triggered := .true.
    valis "world"
  }

  doGlb() => valof{
    _logmsg(conc("hello ",glb));
    valis ()
  }

  main:()=>().
  main() => valof{
    assert ~ triggered!;
    try{
      _jit_compile("#(__pkg__)@glb",0);
      _jit_compile("#(__pkg__)@triggered",0);
      _jit_compile("#(__pkg__)@doGlb",0);
      _jit_compile("#(__pkg__)@conc",2);
    } catch {
      X => showMsg("$(X)")
    };

    doGlb();

    assert triggered!;

    triggered := .false;

    doGlb();

    assert ~ triggered!;
    
    valis ()
  }
}
