test.comp.op{
  import star.
  import star.compiler.operators.

  assert isOperator("?").

  assert \+ isOperator("foo").
}
