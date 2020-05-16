test.df{
  import star.
  import star.script.
  
  CX : (cons[integer],integer) => action[(),integer].
  CX(Is,Lm) => do{
    Cx .= ref 0;

    logMsg("Cx=$(Cx!!), Is=$(Is)");
    for Ix in Is do{
      if Ix<Lm then
  	Cx := Cx!!+Ix
    };

    return Cx!!
  }
  
  IS = [1,2,-3,5,-2,56,10,0].

  CC = CX(IS,4).
    
  TX : (cons[integer],integer) => action[integer,integer].
  TX(Is,Lm) => do{
      Cx .= ref 0;
      
      try{
	VV <- _iter(Is,
		    do{valis ()}, (Ix,_) => action{
			if Ix<Lm then{
			    Cx:=Cx!!+Ix;
			    throw -1
			};
			return ()});
	return Cx!!
      } catch (E) => do{
	  return E
      }
  }
    
  main:()=>action[(),()].
  main()=>do{
    show "CC(IS,2) = $(valof CX(IS,2))";

    show "CC = $(valof CC)";
    show "CC = $(valof CC)";

    assert valof CC == valof CC;
    show "TX(IS,2) = $(valof TX(IS,2))"
  }
}
