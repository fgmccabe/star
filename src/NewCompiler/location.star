star.compiler.location{
  import star.
  import star.pkg.
  public import star.location.
  import star.compiler.misc.

  public pkgLoc:(pkg)=>locn.
  pkgLoc(P) => .locn(pkgName(P),1,0,0,0).

  public lambdaLbl(Lc) => genId((ALc?=Lc??locPkg(ALc)||"")++"λ").
}
