test.cq0{
  import star.core.

  -- Ps:cons[(string,string)].
  -- Ps = [("a","ab"),("b","ab"),("a","c"),("c","aa"),("ab","abc"),
  --           ("de","abc"),("d","de"),("e","de"),
  --           ("f","a"),("g","f")].

  public contract all c,e ~~ folding[c->>e] ::= {
    foldRight:all x ~~ (((e,x)=>x),x,c) => x.
    foldLeft:all x ~~ (((e,x)=>x),x,c) => x.
  }

  public implementation all e ~~ folding[cons[e]->>e] => {
    foldRight(F,U,.nil) => U.
    foldRight(F,U,cons(H,T)) => F(H,foldRight(F,U,T)).

    foldLeft(F,U,.nil) => U.
    foldLeft(F,U,cons(H,T)) => foldLeft(F,F(H,U),T).
  }
  
--  gp : (cons[(string,string)])=>cons[(string,string)].
--  gp(parents) => { (X,Y) | (X,Z) in parents && (Z,Y) in parents}.

  /* gp written by hand as folding query */
  Gf : (cons[(string,string)])=>cons[(string,string)].
  Gf(Ps) => foldLeft(((X,Z),U) =>
      foldLeft(let{.
	  ff((Z,Y),UU) => cons((X,Y),UU).
	  ff(_,UU) => UU
	.} in ff(Pr,U),U,Ps),
      .nil,Ps).

/*
    _iter(Ps,do{return _nil},
        ((Gp,Pr),Cx)=> let{
          sF((Pr,Gc),St)=> do { return _cons((Gp,Gc),St) }
          sF(_,St) => do { return St }
          } in _iter(Ps,do { return Cx },sF)).
  
*/
}

  
