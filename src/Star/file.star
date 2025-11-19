star.file{
  import star.
  import star.capabilities.

  public ioException ::= .ioError | .pastEof | .notFound | .noPerm.

  public implementation display[ioException] => {
    disp(.ioError) => "ioError".
    disp(.pastEof) => "pastEof".
    disp(.notFound) => "notFound".
    disp(.noPerm) => "noPerm".
  }

  public textEncoding ::= .rawEncoding | .utf8Encoding.

  public pickEncoding:(textEncoding) => integer.
  pickEncoding(.rawEncoding) => 0.
  pickEncoding(.utf8Encoding) => 3.

  public getFile:(string) => option[string].
  getFile(Fn) where Fl .= Fn && _file_present(Fl) => valof{
    try{
      valis .some(_get_file(Fl,pickEncoding(.utf8Encoding)))
    } catch {
      _ do valis .none
    }
  }
  getFile(_) default => .none.

  public putFile:(string,string){}.
  putFile(Fn,Content){
    try{
      _put_file(Fn,pickEncoding(.utf8Encoding),Content)
    } catch {
      | _ do {}
    }
  }

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
    } catch {_ do valis .false }
  }

  public isDir:(string) => boolean.
  isDir(D) => _isdir(D).

  public ls:(string) => cons[string].
  ls(D) => (try _ls(D) catch {_ => []}).

  public cwd:()=>string.
  cwd() => _cwd().
}
