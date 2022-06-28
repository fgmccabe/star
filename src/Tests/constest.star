test.cons{
  import star.
  import star.script.


  main:()=>().
  main()=>valof{
    show cons(5,cons(6,.nil));
    
    -- Test equality over cons lists
    assert cons(1,cons(2,cons(3,.nil))) == cons(1,cons(2,cons(3,.nil)));

    -- Test concat
    assert cons(1,cons(2,.nil)) ++ cons(3,.nil) == cons(1,cons(2,cons(3,.nil)));

    valis ()
  }
}
