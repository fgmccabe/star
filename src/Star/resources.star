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

  public logMsg:(string) => ().
  logMsg(Txt) where _ .= _logmsg(Txt) => ().

  public cwd:()=>uri.
  cwd() where U^=parseUri(_cwd()) => U.
}
