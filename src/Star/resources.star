star.resources{
  import star.uri.
  import star.

  public
  getResource:(uri) => option[string].
  getResource(U) where Fn .= getUriPath(U) && _file_present(Fn) => some(_get_file(Fn)).
  getResource(_) default => none.

  public
  putResource:(uri,string)=>().
  putResource(U,Content) => _put_file(getUriPath(U),Content).

  public resourcePresent:(uri)=>boolean.
  resourcePresent(U) => _file_present(getUriPath(U)).

  public newerFile:(uri,uri) => boolean.
  newerFile(F1,F2) where
    P1 .= getUriPath(F1) &&
    P2 .= getUriPath(F2) &&
    _file_present(P1) &&
    _file_present(P2) =>
    _file_modified(P1) > _file_modified(P2).
  newerFile(_,_) default => false.

  public isDir:(string) => boolean.
  isDir(D) => _isdir(D).

  public cwd:()=>uri.
  cwd() where U^=parseUri(_cwd()) => U.

  public searchForRepo:(uri)=>option[uri].
  searchForRepo(U) where P^=parseUri("../") && R ^= parseUri(".star-repo/") =>
    let{
    searchFor(C) where
	RU .= resolveUri(C,R) &&
    P1 .= getUriPath(RU) &&
    _file_present(P1) => some(RU).
    searchFor(C) where
	PU .= resolveUri(C,P) &&
    P1 .= getUriPath(PU) && P1=!="/" =>
      searchFor(PU).
    searchFor(_) default => none
    } in searchFor(U).
}
