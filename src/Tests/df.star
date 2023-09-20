test.df{
  import star.
  import star.assert.
  
  CX : (cons[integer],integer) => integer.
  CX(Is,Lm) => valof{
    Cx = ref 0;

    logMsg("Cx=$(Cx!), Is=$(Is)");
    for Ix in Is do{
      if Ix<Lm then
  	Cx := Cx!+Ix
    };

    valis Cx!
  }
  
  IS = [1,2,-3,5,-2,56,10,0].

  CC = CX(IS,4).
    
  main:()=>().
  main()=>valof{
    show CX(IS,2);

    show CC;
    
    assert CC == CC;
    valis ()
  }
}
