test.di{
  import star.

  Is = cons(1,cons(2,cons(3,cons(4,nil)))).

  Px : action[(),integer].
  Px = _iter(Is,do{valis 1},(Ix,Cx)=>do{valis Ix*Cx}).
  
  show "Px=$(valof Px)".
  
  Rx : action[(),cons[integer]].
  Rx = _iter(Is,do { valis _nil}, (Ix,Cx)=>do {valis _cons(Ix,Cx)}).
  
  show "Rx=$(valof Rx)".

  Fc(I) => action{
	Fx := 1;
	for E in I do {
	  Fx := Fx!*E
	};
	valis Fx!
      }

  show "Fc(Is) = $(valof Fc(Is))".

  FX:(cons[integer]) => action[(),integer].
  FX(I) => action{
    Fx := 1;
    _iter(I,do {valis ()}, (Ix,_) => do { Fx:=Fx!*Ix});
    valis Fx!
  }
  
  show "FX(Is) = $(valof FX(Is))".
}
