test.di{
  import star.
  import test.itr.

  Is = cons(1,cons(2,cons(3,cons(4,nil)))).

  -- Px : action[(),integer].
  -- Px = _iter(Is,do{return 1},(Ix,Cx)=>do{return Ix*Cx}).
  --
  -- show "Px=\(valof Px)".
  --
  -- Rx : action[(),cons[integer]].
  -- Rx = _iter(Is,do { return _nil}, (Ix,Cx)=>do {return _cons(Ix,Cx)}).
  --
  -- show "Rx=\(valof Rx)".

  Fc(I) => action{
	Fx := 1;
	for E in I do {
	  Fx := Fx!*E
	};
	return Fx!
      }

  show "Fc(Is) = \(valof Fc(Is))".

  -- FX:(cons[integer]) => action[(),integer].
  -- FX(I) => action{
  --   Fx := 1;
  --   _iter(I,do {return ()}, (Ix,_) => do { Fx:=Fx!*Ix; return ()});
  --   return Fx!
  -- }
  --
  -- show "FX(Is) = \(valof FX(Is))".
}
