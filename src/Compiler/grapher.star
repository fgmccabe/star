star.compiler.grapher{
  import star.

  import star.resources.
  import star.topsort.
  import star.pkg.
  import star.uri.

  import star.repo.file.

  import star.compiler.catalog.
  import star.compiler.lexer.
  import star.compiler.location.

  scanPkgs:(list[string],fileRepo,catalog,uri,list[pkg])=>list[pkg].



  consistentVersion(defltVersion,_) => true.
  consistentVersion(_,defltVersion) => true.
  consistentVersion(vers(V1),vers(V2)) => V1==V2.
}
