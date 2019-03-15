test.qs{
  import star.
  import star.iterable.

  -- Test simple query rules

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  show "parent = \(parent)".

  /* A comment */

  assert size(parent)==10.

  gp : set[(string,string)].
  gp = { (X,Y) | (X,Z) in parent && (Z,Y) in parent}.

  show "\(gp)".

  pp = [X|(X,"ab") in parent || (X,"de") in parent].

  show "\(pp)".

  pm = [X| (X,Y) in parent && "ab".=Y].

  show "pm=\(pm)".

  -- A different example, filtering positive numbers
  someInts : list[integer].
  someInts = [0,2,-1,-10,4,6,7,-3].

  pos : list[integer].
  pos = { X | X in someInts && X>0 }.
  
  show "\(pos)".

  fact(0)=>1.
  fact(N) where N>0 => fact(N-1)*N.

  ff:list[integer].
  ff = { fact(X) | X in someInts && X>=0}
 
  show "ff = \(ff)".
}
