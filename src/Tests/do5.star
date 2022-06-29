test.do5{
  import star.
  import star.script.

  -- Test abstraction notation

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  main:()=>().
  main() => valof{
    show ({ fst(X) | X in parent}:cons[string]);
    show ({ snd(X) | X in parent}:cons[string]);
    assert size({ X | X in parent}:cons[(string,string)])==size(parent);
    valis ()
  }
}
