test.tagop{
  import star.
  import star.assert.
  import star.bits.

  -- Boundary/sign checks for the tagged-integer arithmetic fast paths
  -- (AND, OR, XOR, ADD, SUB) in the ARM64 JIT's binaryIntOpTagged.

  -- These specifically exercise operandRegister's register-reuse optimization:
  -- X used twice must not be corrupted by the first use reusing its register,
  -- and X op X (both operands the same live variable) must not alias.

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
