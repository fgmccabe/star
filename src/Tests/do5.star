test.do5{
  import star.
  import star.script.

  -- Test abstraction notation

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  main:()=>action[(),()].
  main() => do{
    show [ X | X in parent];
    show [ fst(X) | X in parent];
    show [ snd(X) | X in parent];
    assert size([ X | X in parent])==size(parent)
  }
}
