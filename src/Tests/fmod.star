test.fmod{
  import star.
  import star.assert.

  /* Regression coverage for the ARM64 JIT's sFMod lowering: it previously computed
     a/b (the plain quotient) instead of a - trunc(a/b)*b, because the truncation step
     was missing entirely (see lower.c's frintz-based fix). These specifically exercise
     sign combinations, since a truncation-direction bug hides exactly there. */

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

  /* Exercises readOperandRegister's reuse path for sFMod/sFDiv/sFAbs: X is a live,
     register-resident variable used directly as an operand of three different float
     ops in a row -- must not be corrupted by an earlier op reusing its register (these
     ops are read-only through their operand register, so there is no aliasing hazard
     the way there is for the destructive integer ops, but the reuse path itself needs
     exercising, not just derivation-by-eye). */
  reuseCheck:(float)=>boolean.
  reuseCheck(X) => valof{
    try{
      M = X % 3.0;
      Dv = X / 4.0;
      Ab = _flt_abs(X);
      valis (M - 1.0 < 1.0e-9 && M - 1.0 > -1.0e-9) &&
            (Dv - 2.5 < 1.0e-9 && Dv - 2.5 > -1.0e-9) &&
            (Ab - 10.0 < 1.0e-9 && Ab - 10.0 > -1.0e-9)
    } catch {
      .exception(_) do valis .false
    }
  }

  divZeroMod:(float)=>boolean.
  divZeroMod(X) => valof{
    try{
      _ = X % 0.0;
      valis .false
    } catch {
      .exception(_) do valis .true
    }
  }

  divZeroDiv:(float)=>boolean.
  divZeroDiv(X) => valof{
    try{
      _ = X / 0.0;
      valis .false
    } catch {
      .exception(_) do valis .true
    }
  }

  main:(){}.
  main(){
    try{
      _jit_compile("#(__pkg__)@chk", 3);
      _jit_compile("#(__pkg__)@reuseCheck", 1);
      _jit_compile("#(__pkg__)@divZeroMod", 1);
      _jit_compile("#(__pkg__)@divZeroDiv", 1)
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
    assert chk(0.0, 5.0, 0.0);

    assert reuseCheck(10.0);

    assert divZeroMod(5.0);
    assert divZeroDiv(5.0)
  }
}
