test.hof {
  import star.
  import star.assert.

  map: all a,b ~~ ((a)=>b, cons[a]) => cons[b].
  map(f, .nil) => .nil.
  map(f, .cons(h, t)) => .cons(f(h), map(f, t)).

  double: (integer) => integer.
  double(x) => x * 2.

  main:(){}.
  main() {
    original = .cons(1, .cons(2, .cons(3, .nil)));
    expected = .cons(2, .cons(4, .cons(6, .nil)));
    doubled = map(double, original);

    assert doubled == expected;
  }
}
