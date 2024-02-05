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
      case this suspend .requestIO(()=>~_futureIsResolved(Fut)) in {
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
      case this suspend .requestIO(()=>~_futureIsResolved(Fut)) in {
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
      case this suspend .requestIO(()=>~_futureIsResolved(Fut)) in {
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
      case this suspend .requestIO(()=>~_futureIsResolved(Fut)) in {
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



  

  public rdFileAsync:all e ~~ (this:task[e]), raises ioException|:(string)=> string.
  rdFileAsync(F) => valof{
    try{
      Fut = _getfile_async(F);
      case this suspend .requestIO(()=>~_futureIsResolved(Fut)) in {
	.go_ahead => {
	  if _futureIsResolved(Fut) then{
	    try{
	      valis _futureVal(Fut)
	    } catch errorCode in {
	      | .eof => this retire .retired_
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
}
