test.ar1{
  import star.
  import star.assert.

  doubler:all e ~~ arith[e] |= ((all x ~~ arith[x] |= (x)=>x),e) => e.
  doubler(F,X) => F(F(X)).

  inc:all e ~~ arith[e] |= (e)=>e.
  inc(X) => X+one.

  main:(){}.
  main(){
    show doubler(ζ inc,3);

    assert doubler(ζ inc,3)==5;
  }
}
  
