test.comp.op{
  import star.
  import star.compiler.operators.

  assert isOperator("?").

  assert ~ isOperator("foo").

  assert (720,720,719) ^= isInfixOp("-").
}
