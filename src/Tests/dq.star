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

  /* gp written by hand as iter query */
  Gx : action[(),cons[(string,string)]].
  Gx = _iter(Ps,do{return _nil},
        ((Gp,Pr),Cx)=> let{
          sF((Pr,Gc),St)=> do { return _cons((Gp,Gc),St) }
          sF(_,St) => do { return St }
          } in _iter(Ps,do { return Cx },sF)).
  

  ggp : (cons[(string,string)])=>cons[(string,string)].
  ggp(parents) => { (X,Y) | (X,Z) in parents && (Z,W) in parents && (W,Y) in parents}

  main:()=>action[(),()].
  main()=>do{
    show Ps;
    show "grandparents: $(gp(Ps))";

    show "hand written Gx= $(valof Gx)";

    show "ggp(Ps) = $(ggp(Ps))"
  }
}
