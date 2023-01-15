test.dfr{
  import star.
  import star.script.

  CX : (cons[integer],integer) => integer raises string.
  CX(Is,Lm) => valof{
    Cx = ref 0;

    try{
      logMsg("Cx=$(Cx!), Is=$(Is)");
      for Ix in Is do{
	if Ix<Lm then
	  Cx := Cx!+Ix
      };
      valis Cx!
    } catch{
      (E) => {
	logMsg(E);
	raise E
      }
    }
  }
   
  IS = [1,2,-3,5,-2,56,10,0].

  main:()=>().
  main()=>valof{
    try{
      show CX(IS,2);
    } catch {
      _ => logMsg("something went bad")
    };
    valis ()
  }
}
