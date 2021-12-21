test.lx{
  import star.
  import star.script.

  -- Text lexical features

  /*
   * A block comment
  */

  foo ::= foo{
    'fre$' : string
  }.

  main:()=>action[(),()].
  main()=>action{
    F .= foo{ 'fre$' = "fred" };
    assert F.'fre$' == "fred"
  }
}
