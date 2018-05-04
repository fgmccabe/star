test.cons{
  import star.

  -- Test equality over cons lists

  assert cons(1,cons(2,cons(3,nil))) == cons(1,cons(2,cons(3,nil))).

  -- Test concat

  assert cons(1,cons(2,nil)) ++ cons(3,nil) == cons(1,cons(2,cons(3,nil))).
}
