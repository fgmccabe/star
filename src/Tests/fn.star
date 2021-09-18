test.fn{
  import star.
  import star.finger.
  import star.script.

  S : fingerTree[string].
  S = ["one","two","three","four","five","six","seven","eight","nine","ten"].

  main:()=>action[(),()].
  main()=>action{
    assert "one"^=head(S);
    assert "ten"^=last(S);

    show S;
    show lead(S);
    show last(S);
    show head(S);
    show tail(S)
  }
}
