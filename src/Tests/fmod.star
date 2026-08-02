test.fmod{
  import star.
  import star.assert.

  -- Regression coverage for the ARM64 JIT's sFMod lowering: it previously computed
  -- a/b (the plain quotient) instead of a - trunc(a/b)*b, because the truncation step
  -- was missing entirely (see lower.c's frintz-based fix). These specifically exercise
  -- sign combinations, since a truncation-direction bug hides exactly there.

  chk:(float,float,float)=>boolean.
  chk(A,B,Expect) => valof{
    try{
      R = A % B;
      D = R - Expect;
      valis D < 1.0e-9 && D > -1.0e-9
    } catch {
      .exception(_) do valis .false
    }
  }

  main:(){}.
  main(){
    try{
      _jit_compile("#(__pkg__)@chk", 3)
    } catch {
      X do showMsg("jit compile error: $(X)")
    };

    assert chk(7.0, 2.0, 1.0);
    assert chk(-7.0, 2.0, -1.0);
    assert chk(7.0, -2.0, 1.0);
    assert chk(-7.0, -2.0, -1.0);
    assert chk(5.5, 2.5, 0.5);
    assert chk(1.5, 0.5, 0.0);
    assert chk(10.0, 3.0, 1.0);
    assert chk(0.0, 5.0, 0.0)
  }
}
