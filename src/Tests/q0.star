test.q0{
  import star.
  import star.script.

  -- Test simple query expressions

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  gp : cons[(string,string)].
  gp = { (X,Y) | (X,Z) in parent && (Z,Y) in parent}.

  pp : cons[string].
  pp = {X | (X,"ab") in parent || (X,D) in parent && "de".=D}.

  -- A different example, filtering positive numbers
  someInts : cons[integer].
  someInts = [0,2,-1,-10,4,6,7,-3].

  pos : cons[integer].
  pos = { X | X in someInts && X>0 }.

  main:() => ().
  main() => valof{
    show parent;

    assert size(parent)==10;

    show gp;

    show pp;

    show pos;

    show { (+) <* X <* 0 | X in someInts && X>0 };

    valis ()
  }
}
