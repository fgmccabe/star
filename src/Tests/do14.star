test.do14{
  -- Test throws
  import star.
  import star.script.

  isEven:(integer) => boolean throws string.
  isEven(X) where X%2==0 => .true.
  isEven(X) default => throw "$(X) not even".

  main:()=>().
  main()=>valof{
      try{
	assert isEven(2);
	assert ~ isEven(3);
      } catch {
	M => logMsg("We got #(M)")
      };
    valis ()
  }
  
}
