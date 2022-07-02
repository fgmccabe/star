test.df{
  import star.
  import star.script.
  
  CX : (cons[integer],integer) => result[(),integer].
  CX(Is,Lm) => do{
    Cx .= ref 0;

    logMsg("Cx=$(Cx!), Is=$(Is)");
    for Ix in Is do{
      if Ix<Lm then
  	Cx := Cx!+Ix
    };

    return Cx!
  }
  
  IS = [1,2,-3,5,-2,56,10,0].

  CC = CX(IS,4).
    
  main:()=>().
  main()=>valof{
    try{
      show valof CX(IS,2);

      show valof CC;
      show valof CC;

      assert valof CC == valof CC;
    } catch { _ => logMsg("bad happening")};
    valis ()
  }
}
