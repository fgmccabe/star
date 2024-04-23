test.dfr{
  import star.
  import star.assert.

  CX : raises string |: (cons[integer],integer) => integer.
  CX(Is,Lm) => valof{
    Cx = ref 0;

    try{
      showMsg("Cx=$(Cx!), Is=$(Is)");
      for Ix in Is do{
	if Ix<Lm then
	  Cx := Cx!+Ix
      };
      valis Cx!
    } catch string in {
      (E) => {
	showMsg(E);
	raise E
      }
    }
  }
   
  IS = [1,2,-3,5,-2,56,10,0].

  main:()=>().
  main()=>valof{
    try{
      show CX(IS,2);
    } catch string in {
      _ => showMsg("something went bad")
    };
    valis ()
  }
}
