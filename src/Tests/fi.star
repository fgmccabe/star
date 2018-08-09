test.fi{
  import star.
  import test.fact.

  show "Fact of 3 is \(fact(3)), fact of 4 is \(fact(4))".

  assert "Fact of 3 is 6" == "Fact of 3 is \(fact(3))".
}
