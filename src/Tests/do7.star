test.do7{
  import star.
  import star.script.

  -- Test abstraction notation

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
    ("de","abc"),("d","de"),("e","de"),
    ("f","a"),("g","f")].

  parentOf(X) where (Y,X) in parent => some(Y).
  parentOf(_) default => .none.

  main:()=>action[(),()].
  main() => do{
    show parentOf("ab");
    show parentOf("a");

    assert "a" ^= parentOf("ab");
    assert "ab" ^= parentOf("abc")
  }
}
