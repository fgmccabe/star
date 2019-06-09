star.repo{
  -- Main interface for accessing repositories
  import star.
  import star.pkg.

  public contract all r ~~ repo[r] ::= {
    hasSignature:(r,pkg) => option[string].
    hasCode:(r,pkg) => option[string].
  }
}
