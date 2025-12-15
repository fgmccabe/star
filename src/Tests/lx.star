test.lx{
  import star.
  import star.assert.

  -- Text lexical features

  /*
   * A block comment
  */

  foo ::= foo{
    'fre$' : string
  }.

  main:(){}.
  main(){
    F = foo{ 'fre$' = "fred" };
    assert F.'fre$' == "fred";
  }
}
