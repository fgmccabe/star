star.repo.boot{
  import star.
  import star.parse.
  import star.pkg.
  import star.repo.
  import star.repo.manifest.
  import star.uri.
  import star.resources.

  public bootRepo ::= bootRepo(uri).

  public implementation repo[bootRepo] => {
    hasResource(bootRepo(_),Pkg,Kind) => locateInBootRepo(Pkg,Kind).
  }

  locateInBootRepo(pkg(P,defltVersion),Kind) where _in_manifest(P,"*",Kind) =>
    _locate_in_manifest(P,"*",Kind).
  locateInBootRepo(pkg(P,vers(V)),Kind) where _in_manifest(P,V,Kind) =>
    _locate_in_manifest(P,V,Kind).

  public loadFromRepo:(bootRepo,pkg,string) => option[string].
  loadFromRepo(bootRepo(Root),Pkg,Kind) where
    U ^= locateInBootRepo(Pkg,Kind) &&
    Uri ^= parseUri(U) => getResource(resolveUri(Root,Uri)).
  loadFromRepo(_,_,_) default => none.

  public locateCode:(bootRepo,pkg) => option[string].
  locateCode(Repo,Pkg) => loadFromRepo(Repo,Pkg,"code").

  public implementation display[bootRepo] => {.
    disp(bootRepo(Root)) => ssSeq([ss("boot repo rooted at "),disp(Root)]).
  .}

}
