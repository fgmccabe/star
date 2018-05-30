star.repo{
  -- Main interface for accessing repositories

  public pkg ::= pkg(string,version).

  public version ::= defltVersion | vers(string).

  public contract all r,k ~~ repo[r->>k] ::= {
    hasResource:(r,pkg,k) => boolean.
    accessResource:(r,pkg,k) => string.
  }

   
}
