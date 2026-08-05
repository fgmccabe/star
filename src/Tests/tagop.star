test.tagop{
  import star.
  import star.assert.
  import star.bits.

  /* Boundary/sign checks for the tagged-integer arithmetic fast paths
     (AND, OR, XOR, ADD, SUB) in the ARM64 JIT's binaryIntOpTagged. */

  /* These specifically exercise operandRegister's register-reuse optimization:
     X used twice must not be corrupted by the first use reusing its register,
     and X op X (both operands the same live variable) must not alias. */

  addReuse:(integer) => integer.
  addReuse(X) => valof{
    Y = X + 100;
    Z = X - 1;
    valis Y + Z
  }

  bitReuse:(integer) => integer.
  bitReuse(X) => valof{
    Y = X .&. 6;
    Z = X .|. 6;
    valis Y + Z
  }

  selfAnd:(integer) => integer.
  selfAnd(X) => X .&. X.

  selfOr:(integer) => integer.
  selfOr(X) => X .|. X.

  selfXor:(integer) => integer.
  selfXor(X) => X .^. X.

  selfAdd:(integer) => integer.
  selfAdd(X) => X + X.

  selfSub:(integer) => integer.
  selfSub(X) => X - X.

  /* Exercises binaryIntCompare tagged fast path (no untag, csel routed through tr):
     reused variable across two different comparisons, and same variable as both operands. */
  cmpReuse:(integer) => boolean.
  cmpReuse(X) => valof{
    Y = X == 10;
    Z = X < 10;
    valis Y && ~Z
  }

  selfEq:(integer) => boolean.
  selfEq(X) => X == X.

  selfLt:(integer) => boolean.
  selfLt(X) => X < X.

  selfGe:(integer) => boolean.
  selfGe(X) => X >= X.

  /* Exercises the tagged-bit-pattern fast paths for sIAbs (sub+csneg fixup) and sBNot
     (NOT-then-subtract-1 fixup), both derived to skip the untag/retag round-trip entirely. */
  absCheck:(integer) => integer.
  absCheck(X) => _int_abs(X).

  notCheck:(integer) => integer.
  notCheck(X) => .~. X.

  main:(){}.
  main(){
    -- variable reused across two different ops -- must not be clobbered by the first
    assert(addReuse(10) == 119);   -- Y=110, Z=9
    assert(addReuse(-10) == 79);   -- Y=90, Z=-11
    assert(bitReuse(5) == 11);     -- Y=4, Z=7

    -- same variable as both operands -- must not alias
    assert(selfAnd(21) == 21);
    assert(selfAnd(-1) == -1);
    assert(selfOr(21) == 21);
    assert(selfXor(99) == 0);
    assert(selfXor(0) == 0);
    assert(selfAdd(21) == 42);
    assert(selfAdd(-5) == -10);
    assert(selfSub(21) == 0);
    assert(selfSub(-5) == 0);

    -- variable reused across two different comparisons -- must not be clobbered
    assert(cmpReuse(10));
    assert(~cmpReuse(9));
    assert(~cmpReuse(11));

    -- same variable as both operands of a comparison -- must not alias
    assert(selfEq(21));
    assert(selfEq(-1));
    assert(~selfLt(21));
    assert(~selfLt(-1));
    assert(selfGe(21));
    assert(selfGe(-1));

    -- equality/ordering boundary checks for the tagged compare fast path
    assert(6 == 6);
    assert(~(6 == 7));
    assert((-1) == (-1));
    assert(~((-1) == 1));
    assert(3 < 5);
    assert(~(5 < 3));
    assert((-5) < 3);
    assert(~(3 < (-5)));
    assert(~(5 < 5));
    assert(5 >= 3);
    assert(~(3 >= 5));
    assert(5 >= 5);
    assert(3 >= (-5));
    assert(~((-5) >= 3));

    -- integer absolute value (tagged fast path)
    assert(absCheck(5) == 5);
    assert(absCheck(-5) == 5);
    assert(absCheck(0) == 0);
    assert(absCheck(1) == 1);
    assert(absCheck(-1) == 1);
    assert(absCheck(1000000000) == 1000000000);
    assert(absCheck(-1000000000) == 1000000000);

    -- bitwise complement (tagged fast path)
    assert(notCheck(0) == -1);
    assert(notCheck(-1) == 0);
    assert(notCheck(5) == -6);
    assert(notCheck(-6) == 5);
    assert(notCheck(1000000000) == -1000000001);

    -- bitwise AND
    assert(6 .&. 3 == 2);
    assert(0 .&. 0 == 0);
    assert((-1) .&. 5 == 5);
    assert((-8) .&. (-1) == -8);
    assert(1000000 .&. 0 == 0);
    assert((-1) .&. (-1) == -1);

    -- bitwise OR
    assert(6 .|. 3 == 7);
    assert(0 .|. 0 == 0);
    assert((-1) .|. 5 == -1);
    assert(0 .|. (-8) == -8);
    assert((-1) .|. (-1) == -1);

    -- bitwise XOR
    assert(6 .^. 3 == 5);
    assert(5 .^. 5 == 0);
    assert(0 .^. 7 == 7);
    assert((-1) .^. 0 == -1);
    assert((-1) .^. (-1) == 0);

    -- addition
    assert(2 + 3 == 5);
    assert(0 + 0 == 0);
    assert((-5) + 3 == -2);
    assert((-5) + (-3) == -8);
    assert(1000000000 + 1000000000 == 2000000000);
    assert((-1) + 1 == 0);
    assert((-1) + (-1) == -2);

    -- subtraction
    assert(5 - 3 == 2);
    assert(3 - 5 == -2);
    assert(0 - 0 == 0);
    assert((-5) - (-3) == -2);
    assert((-5) - 3 == -8);
    assert(1000000000 - 2000000000 == -1000000000);
    assert(0 - (-7) == 7)
  }
}
