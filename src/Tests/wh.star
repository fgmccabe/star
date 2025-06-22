test.wh{
  import star.
  import star.assert.

  fct:(integer)=>integer.
  fct(N) => valof{
    F := 1;
    for ix in 0..<N do{
      F := F!*(ix+1)
    };
    valis F!
  }

  main:()=>().
  main() => valof{
    show fct(3);
    assert fct(3)==6;
    assert fct(10) == 3628800;

    valis ()
  }
}  
