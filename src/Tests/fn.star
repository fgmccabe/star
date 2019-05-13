test.fn{
  import star.
  import star.finger.

  S : fingerTree[string].
  S = ["one","two","three","four","five","six","seven","eight","nine","ten"].

  show dump(S).

  assert "one"^=head(S).
  assert "ten"^=last(S).

  show dump(lead(S)).

  show disp(S).
  show disp(lead(S)).
  show disp(last(S)).
  show disp(head(S)).
  show disp(tail(S)).

}
