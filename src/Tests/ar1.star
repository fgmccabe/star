test.ar1{
  import star.
  import star.assert.

  doubler:all e ~~ arith[e] |: ((all x ~~ arith[x] |: (x)=>x),e) => e.
  doubler(F,X) => F(F(X)).

  inc:all e ~~ arith[e] |: (e)=>e.
  inc(X) => X+one.

  main:()=>().
  main()=>valof{
    show doubler(ζinc,3);

    assert doubler(ζinc,3)==5;

    show doubler((((x)=>x+one):(all x ~~ arith[x] |: (x)=>x)),3);
    valis ()
  }
}
  
