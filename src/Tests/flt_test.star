test.flt_test {
  import star.

  public test_div:(){}.
  test_div() {
    try {
      val = 15044000.0 / 1.0e6;
      showMsg("15044000.0 / 1.0e6 = $(val)");
    } catch {
      .exception(M) do showMsg("div exception: $(M)")
    }
  }

  public test_coercion:(){}.
  test_coercion() {
    try {
      fVal = 15044000::float;
      showMsg("15044000::float = $(fVal)");

      showMsg("trunc(15.044) = $(trunc(15.044))");

      truncVal = trunc(15.044)::integer;
      showMsg("trunc(15.044)::integer = $(truncVal)");
    } catch {
      .exception(M) do showMsg("coercion exception: $(M)")
    }
  }

  public test_arith:(){}.
  test_arith() {
    try {
      showMsg("1.5 + 2.5 = $(1.5 + 2.5)");
      showMsg("5.5 - 1.5 = $(5.5 - 1.5)");
      showMsg("2.0 * 3.0 = $(2.0 * 3.0)");
      showMsg("7.0 / 2.0 = $(7.0 / 2.0)");
      showMsg("7.0 % 2.0 = $(7.0 % 2.0)");
    } catch {
      .exception(M) do showMsg("arith exception: $(M)")
    }
  }

  main:(){}.
  main() {
    showMsg("Package: #(__pkg__)");
    try {
      _jit_compile("#(__pkg__)@test_div", 0);
      _jit_compile("#(__pkg__)@test_coercion", 0);
      _jit_compile("#(__pkg__)@test_arith", 0);

      test_div();
      test_coercion();
      test_arith();
    } catch { X do showMsg("error: $(X)") }
  }

  public _main:(cons[string]) => integer.
  _main([]) => valof { main(); valis 0 }
}
