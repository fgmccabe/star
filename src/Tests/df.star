test.df{
  import star.

  IS = [1,2,-3,5,-2,56,10,0].

  -- CX : (list[integer],integer) => action[(),integer].
  -- CX(Is,Lm) => do{
  --   Cx := 0;
  --   for Ix in Is do{
  --     if Ix<Lm then{
  --       Cx:=Cx!+Ix
  --     }
  --   };
  --   return Cx!
  -- }
  
  CX : (list[integer],integer) => action[(),integer].
  CX(Is,Lm) => do{
    Cx := 0;

    for Ix in Is do{
      if Ix<Lm then
	Cx:=Cx!+Ix
      };

    return Cx!
    }

    CC = CX(IS,4).

    show "CC(IS,2) = \(valof CX(IS,2))".

  show "CC = \(valof CC)".
  show "CC = \(valof CC)".

  assert valof CC == valof CC.

  -- TX : (list[integer],integer) => action[(),integer].
  -- TX(Is,Lm) => do{
  --   Cx := 0;
  --
  --   try{
  --     VV <- _iterate(Is,(Ix,_) => action{
  --           if Ix<Lm then
  --             Cx:=Cx!+Ix;
  --           throw (-1)
  --         } :: iterState[integer,()],noneFound)::action[integer,()];
  --     return Cx!
  --   } catch (E) => do{
  --     return E
  --   }
  -- }
  --
  -- show "TX(IS,2) = \(TX(IS,2))".
}
