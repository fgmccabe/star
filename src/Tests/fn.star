test.fn{
  import star.
  import star.finger.
  import star.assert.

  S : fingerTree[string].
  S = ["one","two","three","four","five","six","seven","eight","nine","ten"].

  main:()=>().
  main()=>valof{
    assert "one"?=head(S);
    assert "ten"?=last(S);

    show S;
    show lead(S);
    show last(S);
    show head(S);
    show tail(S);
    valis ()
  }
}
