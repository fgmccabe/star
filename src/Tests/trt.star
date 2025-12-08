test.trt{
  import star.
  import star.assert.

  -- Test try-catch

  neverthrow:(integer){}.
  neverthrow(X){
    show "Im not going to throw $(X)"
  }

  mightThrow:(integer){} throws string.
  mightThrow(X){
    if X<0 then
      throw "negative"
  }

  main:(){}
  main(){
    neverthrow(10);

    try{
      neverthrow(20);

      mightThrow(1);

      mightThrow(-1);

      throw "Hello"
    } catch{
      X do {
	assert X=="negative"
      }
    }
  }
}
