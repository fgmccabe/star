test.do14{
  -- Test throws
  import star.
  import star.assert.

  isEven: (integer) => boolean throws string.
  isEven(X) where (try X%2==0 catch {_ => .false}) => .true.
  isEven(X) default => throw "$(X) not even".

  main:()=>().
  main()=>valof{
      try{
	assert isEven(2);
	assert ~ isEven(3);
      } catch {
	M => showMsg("We got #(M)")
      };
    valis ()
  }
  
}
