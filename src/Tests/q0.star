test.q0{
  import star.
  import star.script.

  -- Test simple query expressions

  parent:list[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  gp : list[(string,string)].
  gp = { (X,Y) | (X,Z) in parent && (Z,Y) in parent}.

  pp = [X|(X,"ab") in parent || (X,"de") in parent].

  -- A different example, filtering positive numbers
  someInts : list[integer].
  someInts = [0,2,-1,-10,4,6,7,-3].

  pos : list[integer].
  pos = { X | X in someInts && X>0 }.

  main:() => action[(),()].
  main() => do{
    show "$(parent)";

    assert size(parent)==10;

    show "$(gp)";

    show "$(pp)";

    show "$(pos)"
  }
}
