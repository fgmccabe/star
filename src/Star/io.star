star.io{
  import star.
  import star.mbox.
  import star.file.

  public ioException ::= .ioError | .pastEof .

  public implementation display[ioException] => {
    disp(.ioError) => "ioError".
    disp(.pastEof) => "pastEof"
  }

  public rdChar:(ioHandle) => char raises ioException.
  rdChar(H) => valof{
    try{
      valis _inchar(H)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdCharAsync:async (ioHandle)=>char raises ioException.
  rdCharAsync(IO) => valof{
    try{
      valis waitforIO(IO,_inchar_async(IO))
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdChars:(ioHandle,integer) => string raises ioException.
  rdChars(H,Cx) => valof{
    try{
      valis _inchars(H,Cx)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdCharsAsync:async (ioHandle,integer)=>string raises ioException.
  rdCharsAsync(IO,Cx) => valof{
    try{
      valis waitforIO(IO,_inchars_async(IO,Cx))
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdLine:(ioHandle) => string raises ioException.
  rdLine(H) => valof{
    try{
      valis _inline(H)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdLineAsync:async (ioHandle)=> string raises ioException.
  rdLineAsync(IO) => valof{
    try{
      valis waitforIO(IO,_inline_async(IO))
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdBytes:(ioHandle,integer) => vect[integer] raises ioException.
  rdBytes(H,Cx) => valof{
    try{
      valis _inbytes(H,Cx)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdBytesAsync:async (ioHandle,integer)=> vect[integer] raises ioException.
  rdBytesAsync(IO,Cx) => valof{
    try{
      valis waitforIO(IO,_inbytes_async(IO,Cx))
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdFile:(string)=> string raises ioException.
  rdFile(F) => valof{
    try{
      valis _get_file(F);
    } catch errorCode in {
      .eof => raise .pastEof
      | _ default => raise .ioError
    }
  }

  public rdFileAsync:async (string)=> string raises ioException.
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

  public wrChar:(ioHandle,char) => () raises ioException.
  wrChar(H,C) => valof{
    try{
      valis _outchar(H,C)
    } catch errorCode in {
      | _ default => raise .ioError
    }
  }

  public wrCharAsync:async (ioHandle,char)=> () raises ioException.
  wrCharAsync(IO,C) => valof{
    try{
      valis waitforIO(IO,_outchar_async(IO,C))
    } catch errorCode in {
      | _ default => raise .ioError
    }
  }

  public wrText:(ioHandle,string) => () raises ioException.
  wrText(H,S) => valof{
    try{
      valis _outtext(H,S)
    } catch errorCode in {
      | _ default => raise .ioError
    }
  }

  public wrTextAsync:async (ioHandle,string)=>() raises ioException.
  wrTextAsync(IO,S) => valof{
    try{
      valis waitforIO(IO,_outtext_async(IO,S))
    } catch errorCode in {
      | _ default => raise .ioError
    }
  }

  public wrFile:(string,string) => () raises ioException.
  wrFile(F,S) => valof{
    try{
      valis _put_file(F,S)
    } catch errorCode in {
      | _ default => raise .ioError
    }
  }

  public wrFileAsync:async (string,string)=> () raises ioException.
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

  waitforIO:all k,e ~~ async (ioHandle,future[k,e])=>k raises e.
  waitforIO(IO,Ft) => valof{
    case this suspend .requestIO(IO,()=>~_futureIsResolved(Ft)) in {
      .go_ahead => {
	if _futureIsResolved(Ft) then{
	  valis _futureVal(Ft)
	} else
	this retire .retired_
      }
      _ =>
	this retire .retired_
    }
  }
  
}
