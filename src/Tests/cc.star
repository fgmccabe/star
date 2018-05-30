test.cc{
  -- Test multiple getConstraints
  import star.

  foo:all x ~~ equality[x],comp[x] |: (x,x)=>boolean.
  foo(X,Y) where X<Y => true.
  foo(X,Y) => X==Y.

  assert foo("alpha","beta").
}
