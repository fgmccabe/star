test.df{
  import star. 
  
  CX : (list[integer],integer) => action[(),integer].
  CX(Is,Lm) => do{
    Cx := 0;

    for Ix in Is do{
      if Ix<Lm then
  	Cx := Cx!+Ix
    };

    return Cx!
  }
  
  IS = [1,2,-3,5,-2,56,10,0].

  CC = CX(IS,4).
    
  show "CC(IS,2) = \(valof CX(IS,2))".

  show "CC = \(valof CC)".
  show "CC = \(valof CC)".

  assert valof CC == valof CC.

  TX : (list[integer],integer) => action[integer,integer].
  TX(Is,Lm) => do{
      Cx := 0;
      
      try{
	VV <- _iter(Is,
		    do{lift ()}, (Ix,_) => action{
			if Ix<Lm then{
			    Cx:=Cx!+Ix;
			    throw -1
			};
			return ()});
	return Cx!
      } catch (E) => do{
	  return E
      }
  }
    
  show "TX(IS,2) = \(valof TX(IS,2))".
}
