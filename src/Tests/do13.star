test.do13{
  import star.
  import star.script.

  maybeFail:(integer)=>result[integer,integer].
  maybeFail(X) => do{
    if X==10 then
      raise 10
    else
      valis X
  }

  main:()=>().
  main() => valof{
    try{
      AA <- maybeFail(5);
      show AA;
      assert AA==5;
      BB .= maybeFail(6);
      show BB;
      assert valof BB == 6;
      XX <- maybeFail(10);
      assert .false  -- never get here
    } catch {
      (Ix) => {
	assert Ix==10
      }
    };
    valis ()
  }
}
    
