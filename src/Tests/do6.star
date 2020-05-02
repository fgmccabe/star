test.do6{
  import star.
  import star.script.

  -- Test abstraction notation

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  main:()=>action[(),()].
  main() => do{
    show [ (X,Y) | (X,Z) in parent && (Z,Y) in parent];

    show [ (X,Y) | (X,Y) in parent || (Y,X) in parent ]
  }
}
