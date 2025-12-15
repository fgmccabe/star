test.cnx{
  import star.
  import star.assert.

  import test.lib.fib.

  -- Test growing stacks
  main:(integer)=>integer.
  main(Ix)=>valof{
    show fib(Ix);
    valis 0
  }

  public _main:(cons[string])=>integer.
  _main([]) => main(10).
  _main([Count]) => main(Count::integer).
  
}
