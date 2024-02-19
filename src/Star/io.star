star.io{
  import star.
  import star.mbox.
  import star.file.

  public ioException ::= .ioError | .pastEof .

  public rdChar:raises ioException |: (ioHandle) => char.
  rdChar(H) => valof{
    try{
      valis _inchar(H)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdCharAsync:all e ~~ (this:task[e]), raises ioException|:(ioHandle)=> char.
  rdCharAsync(IO) => valof{
    try{
      Fut = _inchar_async(IO);
      case this suspend .requestIO(IO,()=>~_futureIsResolved(Fut)) in {
	.go_ahead => {
	  if _futureIsResolved(Fut) then{
	    try{
	      valis _futureVal(Fut)
	    } catch errorCode in {
	      | .eof => raise .pastEof
	      | _ default => raise .ioError
	    }
	  }
	  else
	  this retire .retired_
	}
      }
    }
    catch errorCode in {
      | .eNOPERM => raise .ioError
    }
  }

  public rdChars:raises ioException |: (ioHandle,integer) => string.
  rdChars(H,Cx) => valof{
    try{
      valis _inchars(H,Cx)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdCharsAsync:all e ~~ (this:task[e]), raises ioException|:(ioHandle,integer)=> string.
  rdCharsAsync(IO,Cx) => valof{
    try{
      Fut = _inchars_async(IO,Cx);
      case this suspend .requestIO(IO,()=>~_futureIsResolved(Fut)) in {
	.go_ahead => {
	  if _futureIsResolved(Fut) then{
	    try{
	      valis _futureVal(Fut)
	    } catch errorCode in {
	      | .eof => raise .pastEof
	      | _ default => raise .ioError
	    }
	  }
	  else
	  this retire .retired_
	}
      }
    }
    catch errorCode in {
      | .eNOPERM => raise .ioError
    }
  }

  public rdLine:raises ioException |: (ioHandle) => string.
  rdLine(H) => valof{
    try{
      valis _inline(H)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdLineAsync:all e ~~ (this:task[e]), raises ioException|:(ioHandle)=> string.
  rdLineAsync(IO) => valof{
    try{
      Fut = _inline_async(IO);
      case this suspend .requestIO(IO,()=>~_futureIsResolved(Fut)) in {
	.go_ahead => {
	  if _futureIsResolved(Fut) then{
	    try{
	      valis _futureVal(Fut)
	    } catch errorCode in {
	      | .eof => raise .pastEof
	      | _ default => raise .ioError
	    }
	  }
	  else{
	    logMsg("future wasnt resolved");
	    raise .ioError
	  }
	}
      }
    }
    catch errorCode in {
      | .eNOPERM => raise .ioError
    }
  }

  public rdBytes:raises ioException |: (ioHandle,integer) => vect[integer].
  rdBytes(H,Cx) => valof{
    try{
      valis _inbytes(H,Cx)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdBytesAsync:all e ~~ (this:task[e]), raises ioException|:(ioHandle,integer)=> vect[integer].
  rdBytesAsync(IO,Cx) => valof{
    try{
      Fut = _inbytes_async(IO,Cx);
      case this suspend .requestIO(IO,()=>~_futureIsResolved(Fut)) in {
	.go_ahead => {
	  if _futureIsResolved(Fut) then{
	    try{
	      valis _futureVal(Fut)
	    } catch errorCode in {
	      | .eof => raise .pastEof
	      | _ default => raise .ioError
	    }
	  }
	  else
	  this retire .retired_
	}
      }
    }
    catch errorCode in {
      | .eNOPERM => raise .ioError
    }
  }

  public rdFile:raises ioException|:(string)=> string.
  rdFile(F) => valof{
    try{
      valis _get_file(F);
    } catch errorCode in {
      .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdFileAsync:all e ~~ (this:task[e]), raises ioException|:(string)=> string.
  rdFileAsync(Fl) => valof{
    try{
      In = _openInFile(Fl,3);
      Txt := [];
      try{
	while Ln.=rdCharsAsync(In,1024) do{
	  Txt := [Ln,..Txt!];
	}
      } catch ioException in {
	.pastEof => {
	  logMsg("At eof");
	}
	_ => {
	  logMsg("??");
	  _close(In);
	  raise .ioError
	}
      };

      _close(In);
      valis reverse(Txt!)*
    } catch errorCode in {
      .eof => {
	logMsg("outer eof");
	raise .pastEof
      }
      | _ => {
	logMsg("outer error");
	raise .ioError
      }
    }
  }

  public wrChar:raises ioException |: (ioHandle,char) => ().
  wrChar(H,C) => valof{
    try{
      valis _outchar(H,C)
    } catch errorCode in {
      | _ default => raise .ioError
    }
  }

  public wrCharAsync:all e ~~ (this:task[e]), raises ioException|:(ioHandle,char)=> ().
  wrCharAsync(IO,C) => valof{
    try{
      Fut = _outchar_async(IO,C);
      case this suspend .requestIO(IO,()=>~_futureIsResolved(Fut)) in {
	.go_ahead => {
	  if _futureIsResolved(Fut) then{
	    try{
	      valis _futureVal(Fut)
	    } catch errorCode in {
	      | _ default => raise .ioError
	    }
	  }
	  else
	  this retire .retired_
	}
      }
    }
    catch errorCode in {
      | .eNOPERM => raise .ioError
    }
  }

  public wrText:raises ioException |: (ioHandle,string) => ().
  wrText(H,S) => valof{
    try{
      valis _outtext(H,S)
    } catch errorCode in {
      | _ default => raise .ioError
    }
  }

  public wrTextAsync:all e ~~ (this:task[e]), raises ioException|:
    (ioHandle,string)=> ().
  wrTextAsync(IO,S) => valof{
    try{
      Fut = _outtext_async(IO,S);
      case this suspend .requestIO(IO,()=>~_futureIsResolved(Fut)) in {
	.go_ahead => {
	  if _futureIsResolved(Fut) then{
	    try{
	      valis _futureVal(Fut)
	    } catch errorCode in {
	      | _ default => raise .ioError
	    }
	  }
	  else
	  this retire .retired_
	}
      }
    }
    catch errorCode in {
      | .eNOPERM => raise .ioError
    }
  }

  public wrFile:raises ioException |: (string,string) => ().
  wrFile(F,S) => valof{
    try{
      valis _put_file(F,S)
    } catch errorCode in {
      | _ default => raise .ioError
    }
  }

  public wrFileAsync:all e ~~ (this:task[e]), raises ioException|:
    (string,string)=> ().
  wrFileAsync(F,S) => valof{
    try{
      Ot = _openOutFile(F,3);
      wrTextAsync(Ot,S);
      _close(Ot);
      valis ()
    } catch errorCode in {
      _ default => raise .ioError
    }
  }
}
