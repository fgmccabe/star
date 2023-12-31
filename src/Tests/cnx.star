test.cnx{
  import star.
  import star.assert.

  import test.lib.fib.

  -- Test growing stacks
  main:(integer)=>().
  main(Ix)=>valof{
    show fib(Ix);
    valis ()
  }

  public _main:(cons[string])=>().
  _main([]) => main(10).
  _main([Count]) => main(Count::integer).
  
}
