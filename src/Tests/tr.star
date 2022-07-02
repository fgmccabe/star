test.tr{
  -- Test try-catch expressions

  import star.
  import star.script.

  isOdd:(integer) => result[string,boolean].
  isOdd(X) => (X%2==0 ? bad("not odd") || ok(.true)).

  checkMe:(integer) => boolean.
  checkMe(X) => (try
    valof isOdd(X)
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
  
