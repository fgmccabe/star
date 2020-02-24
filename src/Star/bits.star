star.bits{
  import star.core.

  public ( .|. ) : (integer,integer)=>integer.
  X .|. Y => _bor(X,Y).

  public ( .&. ) : (integer,integer) => integer.
  X .&. Y => _band(X,Y).

  public ( .^. ) : (integer,integer) => integer.
  X .^. Y => _bxor(X,Y).

  public ( .<<. ) : (integer,integer) => integer.
  X .<<. Y => _blsl(X,Y).

  public ( .>>. ) : (integer,integer) => integer.
  X .>>. Y => _blsr(X,Y).

  public ( .>>>. ) : (integer,integer) => integer.
  X .>>>. Y => _basr(X,Y).

  public ( .~. ) : (integer) => integer.
  .~. X => _bnot(X).

}
