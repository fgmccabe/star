test.ex5{
  -- Multiple try scopes, each catching a different exception shape.
  -- No imports. Every throw is caught by a try within the same
  -- function that threw it -- throwing out of a function isn't
  -- exercised here, since that surface syntax is still undecided.

  divByZero ::= .divByZero(integer).
  badIndex ::= .badIndex(integer).
  outOfRange ::= .outOfRange(integer,integer).

  safeDiv:(integer,integer) => integer.
  safeDiv(X,Y) => valof{
    try{
      if _int_eq(Y,0) then
        throw .divByZero(X)
      else
        valis _int_div(X,Y)
    } catch {
      .divByZero(N) do {
        _logmsg("caught divByZero");
        valis _int_minus(0,N)
      }
    }
  }

  -- Two nested scopes: the inner one catches badIndex and re-throws a
  -- different exception type (outOfRange), which only the outer
  -- scope knows how to catch.
  pick:(integer) => integer.
  pick(Ix) => valof{
    try{
      try{
        if _int_lt(Ix,0) then
          throw .badIndex(Ix)
        else if _int_ge(Ix,3) then
          throw .outOfRange(Ix,3)
        else
          valis _int_times(Ix,10)
      } catch {
        .badIndex(I) do {
          _logmsg("caught badIndex, rethrowing as outOfRange");
          throw .outOfRange(I,0)
        }
      }
    } catch {
      .outOfRange(I,Mx) do {
        _logmsg("caught outOfRange");
        valis _int_minus(Mx,I)
      }
    }
  }

  main:(){}.
  main(){
    _logmsg("-- safeDiv(10,0): divByZero caught --");
    R1 = safeDiv(10,0);

    _logmsg("-- safeDiv(10,2): no exception --");
    R2 = safeDiv(10,2);

    _logmsg("-- pick(-1): badIndex rethrown as outOfRange --");
    R3 = pick(-1);

    _logmsg("-- pick(5): outOfRange caught directly --");
    R4 = pick(5);

    _logmsg("-- pick(1): no exception --");
    R5 = pick(1)
  }
}
