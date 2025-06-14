test.jt5{
  import star.
  import star.assert.

  -- test jitting of closures

  lift:all e,f ~~ (option[e],(e)=>f)=>option[f].
  lift(.none,_) => .none.
  lift(.some(X),F) => .some(F(X)).

  dbl:(integer)=>integer.
  dbl(X) => _int_plus(X,X).

  main:(integer)=>().
  main(C) => valof{
    A = .none;
    B = .some(3);

    show "A=$(A)";
    show "B=$(B)";

    show "lift A=$(lift(A,dbl))";
    show "lift B=$(lift(B,dbl))";

    assert lift(B,dbl) == .some(6);
    assert lift(lift(B,dbl),dbl) == .some(12);

    try{
      _jit_compile("#(__pkg__)@lift",2);
      _jit_compile("#(__pkg__)@dbl",1);
      _jit_compile("#(__pkg__)@dbl^",2);
    } catch {
      X => showMsg("$(X)")
    };

    show "lift A=$(lift(A,dbl))";
    show "lift B=$(lift(B,dbl))";

    assert lift(B,dbl) == .some(6);
    assert lift(lift(B,dbl),dbl) == .some(12);

    valis ()
  }
}  
