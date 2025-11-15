test.do13{
  import star.
  import star.assert.

  maybeFail: (integer)=>integer throws integer.
  maybeFail(X) => valof{
    if X==10 then
      throw 10
    else
      valis X
  }

  main:(){}.
  main(){
    try{
      AA = maybeFail(5);
      show AA;
      assert AA==5;
      BB = maybeFail(6);
      show BB;
      assert BB == 6;
      XX = maybeFail(10);
      assert .false  -- never get here
    } catch {
      (Ix) do {
	assert Ix==10
      }
    }
  }
}
    
