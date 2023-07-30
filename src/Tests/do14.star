test.do14{
  -- Test raises
  import star.
  import star.script.

  isEven: raises string |: (integer) => boolean.
  isEven(X) where X%2==0 => .true.
  isEven(X) default => raise "$(X) not even".

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
