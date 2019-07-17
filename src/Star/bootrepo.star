star.repo.boot{
  import star.
  import star.pkg.
  import star.repo.
  import star.resources.
  import star.uri.

  locateInBootRepo(pkg(P,defltVersion),Kind) where _in_manifest(P,"*",Kind) =>
    some(_locate_in_manifest(P,"*",Kind)).
  locateInBootRepo(pkg(P,vers(V)),Kind) where _in_manifest(P,V,Kind) =>
    some(_locate_in_manifest(P,V,Kind)).
  locateInBootRepo(_,_) default => none.

  public btRepo:(uri) => repository.
  btRepo(Root) => {
    hasSignature(P) => locateInBootRepo(P,"signature").

    hasCode(Pkg) where 
	U ^= locateInBootRepo(Pkg,"code") &&
	Uri ^= parseUri(U) &&
	RU ^= resolveUri(Root,Uri) => getResource(RU).
    hasCode(_) default => none.
  }.
    
}
