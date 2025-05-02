test.tr{
  -- Test try-catch expressions

  import star.
  import star.assert.

  isOdd:(integer) => boolean throws string.
  isOdd(X) => (X.&.1==0 ?? (throw "not odd") || .true).

  checkMe:(integer) => boolean.
  checkMe(X) => (try
    isOdd(X)
    catch {
      _ => .false
    }).

  main:()=>().
  main()=>valof{
    show checkMe(3);
    show checkMe(4);

    assert checkMe(3);
    assert ~checkMe(4);
    valis ()
  }
}
  
