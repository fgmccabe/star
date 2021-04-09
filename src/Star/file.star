star.file{
  import star.
  import star.capabilities.

  public
  getFile:(string) => option[string].
  getFile(Fn) where _file_present(Fn) => some(_get_file(Fn)).
  getFile(_) default => .none.

  public
  putFile:(string,string)=>().
  putFile(Fn,Content) => _put_file(Fn,Content).


}
