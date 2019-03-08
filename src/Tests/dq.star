test.dq{
  import star.
  import test.itr.

  -- Test simple query expressions

  Ps:cons[(string,string)].
  Ps = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
            ("de","abc"),("d","de"),("e","de"),
            ("f","a"),("g","f")].

  show "\(Ps)".


  -- gp : (cons[(string,string)])=>cons[(string,string)].
  -- gp(parents) => { (X,Y) | (X,Z) in parents && (Z,Y) in parents}.
  --
  -- show "\(gp(Ps))".

  -- /* gp written by hand as iter query */
  -- Gx : action[(),list[(string,string)]].
  -- Gx = _iter(Ps,do{return _nil},
  --       ((Gp,Pr),Cx)=> let{
  --         sF((Pr,Gc),St)=> do { return _cons((Gp,Gc),St) }
  --         sF(_,St) => do { return St }
  --         } in _iter(Ps,do { return Cx },sF)).
  --
  -- show "Gx=\(valof Gx)".

  ggp : (cons[(string,string)])=>cons[(string,string)].
  ggp(parents) => { (X,Y) | (X,Z) in parents && (Z,W) in parents && (W,Y) in parents}


  show "ggp(Ps) = \(ggp(Ps))".
}
