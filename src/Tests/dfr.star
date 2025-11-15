test.dfr{
  import star.
  import star.assert.

  CX : (cons[integer],integer) => integer throws string.
  CX(Is,Lm) => valof{
    Cx = ref 0;

    try{
      showMsg("Cx=$(Cx!), Is=$(Is)");
      for Ix in Is do{
	if Ix<Lm then
	  Cx := Cx!+Ix
      };
      valis Cx!
    } catch {
      (E) do {
	showMsg(E);
	throw E
      }
    }
  }
   
  IS = [1,2,-3,5,-2,56,10,0].

  main:(){}.
  main(){
    try{
      show CX(IS,2);
    } catch {
      _ do showMsg("something went bad")
    };
  }
}
