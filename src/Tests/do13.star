test.do13{
  import star.
  import star.script.

  maybeFail:(integer)=>integer throws integer.
  maybeFail(X) => valof{
    if X==10 then
      throw 10
    else
      valis X
  }

  main:()=>().
  main() => valof{
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
      (Ix) => {
	assert Ix==10
      }
    };
    valis ()
  }
}
    
