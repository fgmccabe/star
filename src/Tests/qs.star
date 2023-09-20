test.qs{
  import star.
  import star.iterable.
  import star.assert.

  -- Test simple query rules

  parent:cons[(string,string)].
  parent = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  /* A comment */

  gp : set[(string,string)].
  gp = { (X,Y) | (X,Z) in parent && (Z,Y) in parent}.

  pp : cons[string].
  pp = {X|(X,"ab") in parent || (X,"de") in parent}.

  pm:cons[string].
  pm = {X | (X,Y) in parent && "ab".=Y}.

  -- A different example, filtering positive numbers
  someInts : cons[integer].
  someInts = [0,2,-1,-10,4,6,7,-3].

  pos : cons[integer].
  pos = { X | X in someInts && X>0 }.
  
  fact(0)=>1.
  fact(N) where N>0 => fact(N-1)*N.

  ff:cons[integer].
  ff = { fact(X) | X in someInts && X>=0}

  main:() => ().
  main() => valof{
    show parent;
    show gp;

    show ff;

    assert size(parent)==10;

    show pp;

    show pos;
    show pm;

    valis ()
  }
}
