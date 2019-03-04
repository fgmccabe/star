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

    VV = _iterate(Is,(Ix,St) => valof action{
            if Ix<Lm then
              Cx:=Cx!+Ix;
            return St
          },continueWith(()))::action[(),()];

    return Cx!
  }

  CC = CX(IS,4).

  show "CC(IS,2) = \(valof CX(IS,2))".

  show "CC = \(valof CC)".
  show "CC = \(valof CC)".

  assert valof CC == valof CC.
}
