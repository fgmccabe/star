test.dq{
  import star.
  import star.script.

  -- Test simple query expressions

  Ps:cons[(string,string)].
  Ps = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].


  gp : (cons[(string,string)])=>cons[(string,string)].
  gp(parents) => { (X,Y) | (X,Z) in parents && (Z,Y) in parents}.

  ggp : (cons[(string,string)])=>cons[(string,string)].
  ggp(parents) => { (X,Y) | (X,Z) in parents && (Z,W) in parents && (W,Y) in parents}

  main:()=>().
  main()=>valof{
    show Ps;
    show gp(Ps);
    show ggp(Ps);
    valis ()
  }
}
