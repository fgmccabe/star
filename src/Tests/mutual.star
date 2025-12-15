test.mutual {
  import star.
  import star.assert.

  is_even: (integer) => boolean.
  is_even(0) => .true.
  is_even(n) => is_odd(n - 1).

  is_odd: (integer) => boolean.
  is_odd(0) => .false.
  is_odd(n) => is_even(n - 1).

  main:(){}.
  main() {
    assert is_even(4);
    assert is_odd(5);
  }
}
