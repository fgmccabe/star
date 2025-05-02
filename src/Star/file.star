star.file{
  import star.
  import star.capabilities.

  public getFile:(string) => option[string].
  getFile(Fn) where Fl .= Fn && _file_present(Fl) => valof{
    try{
      valis .some(_get_file(Fl))
    } catch {
      .eNOTFND => valis .none
    }
  }
  getFile(_) default => .none.

  public putFile:(string,string)=>().
  putFile(Fn,Content) =>
    (try _put_file(Fn,Content) catch {_ => ()}).

  public filePresent:(string) => boolean.
  filePresent(Fn) => _file_present(Fn).

  public newerFile:(string,string) => boolean.
  newerFile(F1,F2) => valof{
    try{
      if _file_present(F1) &&
	  _file_present(F2) then
	valis _file_modified(F1) > _file_modified(F2)
      else
      valis .false
    } catch {_ => valis .false }
  }

  public isDir:(string) => boolean.
  isDir(D) => _isdir(D).

  public ls:(string) => cons[string].
  ls(D) => (try _ls(D) catch {_ => []}).

  public cwd:()=>string.
  cwd() => _cwd().
  
}
