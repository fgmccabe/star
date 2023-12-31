test.v0{
  import star.
  import star.vector.
  import star.assert.

  main:()=>().
  main()=>valof{
    VV := nullV();
    AA = (["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q"]:cons[string]);
    for A in AA do{
      VV := appnd(VV!,A);
      logMsg("After adding #(A), VV=$(VV!) $(isFullVect(VV!))")
    };

    show vindex(VV!,0);
    show vindex(VV!,1);
    show vindex(VV!,2);
    show vindex(VV!,3);
    show vindex(VV!,4);
    show vindex(VV!,5);
    show vindex(VV!,6);
    show vindex(VV!,7);
    show vindex(VV!,8);
    show vindex(VV!,9);
    show vindex(VV!,10);

    assert "A" ?= vindex(VV!,0);
    assert "B" ?= vindex(VV!,1);
    assert "C" ?= vindex(VV!,2);
    assert "D" ?= vindex(VV!,3);
    assert "E" ?= vindex(VV!,4);
    assert "F" ?= vindex(VV!,5);
    assert "G" ?= vindex(VV!,6);
    assert "H" ?= vindex(VV!,7);
    assert "I" ?= vindex(VV!,8);
    assert "J" ?= vindex(VV!,9);
    assert ~ _ ?= vindex(VV!,20);

    TT = AA::vect[string];

    show TT;

    for E in VV! do{
      show E
    };

    for E in TT do{
      show E
    };

    assert size(TT)==size(AA);
    assert isEmpty(.nil::vect[integer]);

    assert VV!==TT;

    for ix in .range(0,size(VV!),1) do{
      show vindex(VV!,ix)
    };
    
    for ix in .range(0,size(VV!),1) do{
      if Ex ?= vindex(VV!,ix) then
	VV := vupdate(VV!,ix,Ex++"*");
    };

    show VV!;

    for ix in .range(0,size(VV!),1) do{
      show vdelete(VV!,ix)
    };

    for ix in .range(0,size(VV!),1) do{
      VV := vdelete(VV!,0);
      show VV!
    };

    show TT//((X)=>X++X);

    show ({ Ix | Ix in .range(0,100,1) }:vect[integer]);

    valis ()
  }
}
