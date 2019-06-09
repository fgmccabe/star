star.repo.boot{
  import star.
  import star.pkg.
  import star.repo.
  import star.resources.
  import star.uri.

  public bootRepo ::= bootRepo(uri).

  public implementation repo[bootRepo] => {
    hasSignature(_,Pkg) => locateInBootRepo(Pkg,"signature").

    hasCode(bootRepo(Root),Pkg) where 
	U ^= locateInBootRepo(Pkg,"code") &&
	Uri ^= parseUri(U) &&
	RU ^= resolveUri(Root,Uri) => getResource(RU).
    hasCode(_,_) default => none.
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
