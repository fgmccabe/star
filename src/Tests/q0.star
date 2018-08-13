test.q0{
  import star.
  import star.iterable.

  -- Test simple query rules

  parent:list[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  show "\(parent)".

  assert size(parent)==10.

  gp = { (X,Y) | (X,Z) in parent && (Z,Y) in parent}.

}
