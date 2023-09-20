test.do14{
  -- Test raises
  import star.
  import star.assert.

  isEven: raises string |: (integer) => boolean.
  isEven(X) where (try X%2==0 catch exception in {_ => .false}) => .true.
  isEven(X) default => raise "$(X) not even".

  main:()=>().
  main()=>valof{
      try{
	assert isEven(2);
	assert ~ isEven(3);
      } catch string in {
	M => logMsg("We got #(M)")
      };
    valis ()
  }
  
}
