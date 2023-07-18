star.file{
  import star.
  import star.capabilities.

  public getFile:(string) => option[string].
  getFile(Fn) where Fl .= Fn && _file_present(Fl) => .some(_get_file(Fl)).
  getFile(_) default => .none.

  public putFile:(string,string)=>().
  putFile(Fn,Content) => _put_file(Fn,Content).

  public filePresent:(string) => boolean.
  filePresent(Fn) => _file_present(Fn).

  public newerFile:(string,string) => boolean.
  newerFile(F1,F2) where
      _file_present(F1) &&
      _file_present(F2) =>
    _file_modified(F1) > _file_modified(F2).
  newerFile(_,_) default => .false.

  public isDir:(string) => boolean.
  isDir(D) => _isdir(D).

  public ls:(string) => cons[string].
  ls(D) => _ls(D).

  public cwd:()=>string.
  cwd() => _cwd().
  
}
