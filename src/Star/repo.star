star.repo{
  -- Main interface for accessing repositories
  import star.
  import star.pkg.

  public contract all r ~~ repo[r] ::= {
    hasResource:(r,pkg,string) => option[string].
  }
}
