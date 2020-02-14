test.sl{
  import star.
  import star.script.

  -- test slice and splice operators
  F = "foobar".

  main:()=>action[(),()].
  main() => action{
    show F[3:6];
    assert F[3:6] == "bar"
  }
}
