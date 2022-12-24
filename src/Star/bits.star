star.bits{
  import star.core.

  public contract all x ~~ bits[x] ::= {
    (.|.):(x,x)=>x.
    (.&.):(x,x)=>x.
    (.^.):(x,x)=>x.
    (.~.):(x)=>x.
  }

  public implementation bits[integer] => {
    X .|. Y => _bor(X,Y).
    X .&. Y => _band(X,Y).
    X .^. Y => _bxor(X,Y).
    .~. X => _bnot(X).
  }

  public implementation bits[bigint] => {
    X .|. Y => _big_bitor(X,Y).
    X .&. Y => _big_bitand(X,Y).
    X .^. Y => _big_bitxor(X,Y).
    .~. X => _big_bitnot(X).
  }

  public ( .<<. ) : (integer,integer) => integer.
  X .<<. Y => _blsl(X,Y).

  public ( .>>. ) : (integer,integer) => integer.
  X .>>. Y => _blsr(X,Y).

  public ( .>>>. ) : (integer,integer) => integer.
  X .>>>. Y => _basr(X,Y).
}
