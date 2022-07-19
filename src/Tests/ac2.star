test.ac2{
  import star.
  import star.script.

  -- Another test of for loops
  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
    ("de","abc"),("d","de"),("e","de"),
    ("f","a"),("g","f")].

  gps:(cons[(string,string)])=>cons[(string,string)].
  gps(Ps) => valof{
    gs := [];
    for (X,Y) in Ps do{
      for (Y,Z) in Ps do{
	gs := [(X,Z),..gs!]
      }
    };
    valis gs!
  }

  main:()=>().
  main() => valof{
    show gps(parent);
    show [|gps(parent)|];
    assert [|gps(parent)|]=<[|parent|];
    valis ()
  }
}

