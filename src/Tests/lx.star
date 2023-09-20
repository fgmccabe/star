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

  main:()=>().
  main()=>valof{
    F = foo{ 'fre$' = "fred" };
    assert F.'fre$' == "fred";
    valis ()
  }
}
