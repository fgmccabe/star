test.q0{
  import star.

  -- Test simple query expressions

  parent:list[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  show "$(parent)".

  assert size(parent)==10.

  gp : list[(string,string)].
  gp = { (X,Y) | (X,Z) in parent && (Z,Y) in parent}.

  show "$(gp)".

  pp = [X|(X,"ab") in parent || (X,"de") in parent].

  show "$(pp)".

  -- A different example, filtering positive numbers
  someInts : list[integer].
  someInts = [0,2,-1,-10,4,6,7,-3].

  pos : list[integer].
  pos = { X | X in someInts && X>0 }.

  show "$(pos)".

}
