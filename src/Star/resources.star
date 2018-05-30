star.resources{
  import star.uri.
  import star.

  public
  getResource:(uri) => string.
  getResource(U) => _get_file(getUriPath(U)).

  public
  putResource:(uri,string)=>().
  putResource(U,Content) => _put_file(getUriPath(U),Content).

  public resourcePresent:(uri)=>boolean.
  resourcePresent(U) => _file_present(getUriPath(U)).
}
