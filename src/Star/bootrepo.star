star.repo.boot{
  import star.
  import star.pkg.
  import star.repo.
  import star.uri.

  public bootRepo ::= bootRepo(uri).

  public implementation repo[bootRepo] => {
    hasResource(bootRepo(_),Pkg,Kind) => locateInBootRepo(Pkg,Kind).
    repoRoot(bootRepo(Root)) => Root.
  }

  locateInBootRepo(pkg(P,defltVersion),Kind) where _in_manifest(P,"*",Kind) =>
    some(_locate_in_manifest(P,"*",Kind)).
  locateInBootRepo(pkg(P,vers(V)),Kind) where _in_manifest(P,V,Kind) =>
    some(_locate_in_manifest(P,V,Kind)).
  locateInBootRepo(_,_) default => none.

  public implementation display[bootRepo] => {.
    disp(bootRepo(Root)) => ssSeq([ss("boot repo rooted at "),disp(Root)]).
  .}
}
