test.do7{
  import star.
  import star.script.

  -- Test abstraction notation

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
    ("de","abc"),("d","de"),("e","de"),
    ("f","a"),("g","f")].

  parentOf(X) => {!Y | (Y,X) in parent !}.

  check(X) where _ ^= {! () | (X,_) in parent !} => .true.
  check(_) default => .false.
  
  main:()=>action[(),()].
  main() => action{
    show parentOf("ab");
    show parentOf("a");

    assert ("a" ^= parentOf("ab") || "b" ^= parentOf("ab"));
    assert ("ab" ^= parentOf("abc") || "de" ^= parentOf("abc"));

    assert check("a");
    assert ~check("z")
  }
}
