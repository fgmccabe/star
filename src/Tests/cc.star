test.cc{
  -- Test multiple getConstraints
  import star.
  import star.assert.

  foo:all x ~~ equality[x],comp[x] |: (x,x)=>boolean.
  foo(X,Y) where X<Y => .true.
  foo(X,Y) => X==Y.

  main:()=>().
  main()=>valof{
    assert foo("alpha","beta");
    valis ()
  }
}
