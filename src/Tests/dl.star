test.dl{
  import star.
  import test.fact.

  -- Test iteration constructs

  SS : (integer,list[integer]) => action[(),boolean].
  SS(Ix,Lx) => do {
    if Ix in Lx then
      return true
      else
      return false
    }

  show "SS(1,[3,2,1])=$(valof SS(1,[3,2,1]))".
  show "SS(4,[3,2,1])=$(valof SS(4,[3,2,1]))".

  ST : (integer,list[integer]) => action[(),boolean].
  ST(Ix,Lx) => do {
    valis Ix in Lx
  }

  show "ST(1,[3,2,1])=$(valof ST(1,[3,2,1]))".
  show "ST(4,[3,2,1])=$(valof ST(4,[3,2,1]))".

  SM:(integer,list[integer]) => boolean.
  SM(X,L) => X in L.

  assert SM(3,[1,2,3]).

  FF:(integer)=>action[(),integer].
  FF(Lx)=> action{
    Cx := 1;
    Fx := 1;
    while Cx!=<Lx do {
      Fx := Fx! * Cx!;
      Cx := Cx! + 1
    };
    return Fx!
  }
  show "FF(3) = $(valof FF(3))".
  show "FF(4) = $(valof FF(4))".

  FR = FF(4).

  show "FR = $(valof FR)".
  show "FR = $(valof FR)".

  II(Lx) =>
    return () >>=
      ((_) =>
        ((Cx)=>
            let {
              loop:()=>action[(),integer].
              loop() where Cx! =< Lx =>
                do{
                  Cx := Cx! + 1;
                  loop()
                }
              loop() => return Cx!
            } in loop()
            )(_cell(1))).

  show "II(3) = $(valof II(3))".

  PP:(integer)=>action[(),integer].
  PP(Lx) =>
    return () >>=
      ((_) =>
        ((Cx)=>
          ((Fx) =>
            let {
              loop:()=>action[(),integer].
              loop() where Cx! =< Lx =>
                do{
                  Fx := Fx! * Cx!;
                  Cx := Cx! + 1;
                  loop()
                }
              loop() => return Fx!
            } in loop()
            )
            (_cell(1))
             )(_cell(1))).

  show "PP(4) = $(valof PP(4))".

  QQ:(integer)=>action[(),integer].
  QQ(Lx) =>
    do{
      Cx := 1;
      Fx := 1;

      while Cx! =< Lx do{
	Fx := Fx! * Cx!;
	Cx := Cx! + 1
      };
      valis Fx!
    }

  show "QQ(3) = $(valof QQ(3))".
  show "QQ(4) = $(valof QQ(4))".
  show "QQ(2) = $(valof QQ(2))".

  QR = QQ(4).
  show "QR = $(valof QR)".
  show "QR = $(valof QR)".
}
