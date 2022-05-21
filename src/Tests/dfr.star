test.dfr{
  import star.
  import star.script.


  CX : (cons[integer],integer) => result[string,integer].
  CX(Is,Lm) => do{
    Cx .= ref 0;

    try{
      logMsg("Cx=$(Cx!), Is=$(Is)");
      for Ix in Is do{
	if Ix<Lm then
	  Cx := Cx!+Ix
      };
      valis Cx!
    } catch(E) => do{
      logMsg(E);
      raise E
    }
  }
   

  IS = [1,2,-3,5,-2,56,10,0].

  main:()=>action[(),()].
  main()=>action{
    show valof CX(IS,2);
  }
}
