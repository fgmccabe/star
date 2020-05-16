test.dl{
  import star.
  import star.script.
  import test.fact.

  -- Test iteration constructs

  SS : (integer,cons[integer]) => action[(),boolean].
  SS(Ix,Lx) => do {
    if Ix in Lx then
      return .true
      else
      return .false
    }


  ST : (integer,cons[integer]) => action[(),boolean].
  ST(Ix,Lx) => do {
    valis Ix in Lx
  }

  SM:(integer,cons[integer]) => boolean.
  SM(X,L) => X in L.

  FF:(integer)=>action[(),integer].
  FF(Lx)=> action{
    Cx .= ref 1;
    Fx .= ref 1;
    while Cx!!=<Lx do {
      Fx := Fx!! * Cx!!;
      Cx := Cx!! + 1
    };
    return Fx!!
  }

  II(Lx) =>
    return () >>=
      ((_) =>
        ((Cx)=>
            let {
              loop:()=>action[(),integer].
              loop() where Cx!! =< Lx =>
                do{
                  Cx := Cx!! + 1;
                  loop()
                }
              loop() => return Cx!!
            } in loop()
            )(_cell(1))).

  PP:(integer)=>action[(),integer].
  PP(Lx) =>
    return () >>=
      ((_) =>
        ((Cx)=>
          ((Fx) =>
            let {
              loop:()=>action[(),integer].
              loop() where Cx!! =< Lx =>
                do{
                  Fx := Fx!! * Cx!!;
                  Cx := Cx!! + 1;
                  loop()
                }
              loop() => return Fx!!
            } in loop()
            )
            (_cell(1))
             )(_cell(1))).


  QQ:(integer)=>action[(),integer].
  QQ(Lx) =>
    do{
      Cx .= ref 1;
      Fx .= ref 1;

      while Cx!! =< Lx do{
	Fx := Fx!! * Cx!!;
	Cx := Cx!! + 1
      };
      valis Fx!!
    }

  main:()=>action[(),()].
  main()=>do{
    assert SM(3,[1,2,3]);
    
    show "SS(1,[3,2,1])=$(valof SS(1,[3,2,1]))";
    show "SS(4,[3,2,1])=$(valof SS(4,[3,2,1]))";

    show "ST(1,[3,2,1])=$(valof ST(1,[3,2,1]))";
    show "ST(4,[3,2,1])=$(valof ST(4,[3,2,1]))";

    show "PP(4) = $(valof PP(4))";

    show "QQ(3) = $(valof QQ(3))";
    show "QQ(4) = $(valof QQ(4))";
    show "QQ(2) = $(valof QQ(2))";

    QR .= QQ(4);
    show "QR = $(valof QR)";
    show "QR = $(valof QR)";

    FR .= FF(4);

    show "FR = $(valof FR)";
    show "FR = $(valof FR)";

    show "FF(3) = $(valof FF(3))";
    show "FF(4) = $(valof FF(4))";

    show "II(3) = $(valof II(3))"
  }
}
