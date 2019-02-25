star.repo{
  -- Main interface for accessing repositories
  import star.
  import star.pkg.
  import star.uri.

  public contract all r ~~ repo[r] ::= {
    hasResource:(r,pkg,string) => option[string].

    repoRoot:(r) => uri.
  }

  public locateCode:all r ~~ repo[r] |: (r,pkg) => option[string].
  locateCode(Repo,Pkg) => hasResource(Repo,Pkg,"code").
}
