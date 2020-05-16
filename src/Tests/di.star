test.di{
  import star.
  import star.script.

  Is = cons(1,cons(2,cons(3,cons(4,.nil)))).

  Px : action[(),integer].
  Px = _iter(Is,do{valis 1},(Ix,Cx)=>do{valis Ix*Cx}).
  
  Rx : action[(),cons[integer]].
  Rx = _iter(Is,do { valis _nil}, (Ix,Cx)=>do {valis _cons(Ix,Cx)}).
  
  Fc(I) => action{
	Fx .= ref 1;
	for E in I do {
	  Fx := Fx!! * E
	};
	valis Fx!!
      }

  FX:(cons[integer]) => action[(),integer].
  FX(I) => action{
    Fx .= ref 1;
    _iter(I,do {valis ()}, (Ix,_) => do { Fx:=Fx!!*Ix});
    valis Fx!!
  }

  main:()=>action[(),()].
  main()=>do{
    show "Px=$(valof Px)";
    show "Rx=$(valof Rx)";

    show "Fc(Is) = $(valof Fc(Is))";
  
    show "FX(Is) = $(valof FX(Is))"
  }
}
