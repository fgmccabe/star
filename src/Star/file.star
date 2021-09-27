star.file{
  import star.
  import star.capabilities.

  public getFile:(string) => option[string].
  getFile(Fn) where Fl .= _str_fltn(Fn) && _file_present(Fl) => some(chrs_(_get_file(Fl))).
  getFile(_) default => .none.

  public putFile:(string,string)=>().
  putFile(Fn,Content) => _put_file(_str_fltn(Fn),_str_fltn(Content)).

  public filePresent:(string) => boolean.
  filePresent(Fn) => _file_present(_str_fltn(Fn)).

  public newerFile:(string,string) => boolean.
  newerFile(F1,F2) where
      Fn1 .= _str_fltn(F1) && Fn2 .= _str_fltn(F2) &&
      _file_present(Fn1) &&
      _file_present(Fn2) =>
    _file_modified(Fn1) > _file_modified(Fn2).
  newerFile(_,_) default => .false.

  public isDir:(string) => boolean.
  isDir(D) => _isdir(_str_fltn(D)).
  
  
}
