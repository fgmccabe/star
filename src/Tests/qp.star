test.qp{
  import star.
  import star.script.

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
    ("de","abc"),("d","de"),("e","de"),
    ("f","a"),("g","f")].

  m:cons[string].
  m = ["a","ab","d","f"].

  fatherOf(X) => {! F | (F,X) in parent && F in m!}.

  main:()=>().
  main()=>valof{
    show fatherOf("ab");
    assert "a"?=fatherOf("ab");
    valis ()
  }
}

