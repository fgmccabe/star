star.io{
  import star.
  import star.mbox.
  import star.file.

  public ioException ::= .ioError | .pastEof .

  public implementation display[ioException] => {
    disp(.ioError) => "ioError".
    disp(.pastEof) => "pastEof"
  }

  public rdChar:(ioHandle) => char throws ioException.
  rdChar(H) => valof{
    try{
      valis _inchar(H)
    } catch {
      | .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  public rdCharAsync:async (ioHandle)=>char throws ioException.
  rdCharAsync(IO) => valof{
    try{
      valis waitforIO(IO,_inchar_async(IO))
    } catch {
      | .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  public rdChars:(ioHandle,integer) => string throws ioException.
  rdChars(H,Cx) => valof{
    try{
      valis _inchars(H,Cx)
    } catch {
      | .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  public rdCharsAsync:async (ioHandle,integer)=>string throws ioException.
  rdCharsAsync(IO,Cx) => valof{
    try{
      valis waitforIO(IO,_inchars_async(IO,Cx))
    } catch {
      | .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  public rdLine:(ioHandle) => string throws ioException.
  rdLine(H) => valof{
    try{
      valis _inline(H)
    } catch {
      | .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  public rdLineAsync:async (ioHandle)=> string throws ioException.
  rdLineAsync(IO) => valof{
    try{
      valis waitforIO(IO,_inline_async(IO))
    } catch {
      | .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  public rdBytes:(ioHandle,integer) => vect[integer] throws ioException.
  rdBytes(H,Cx) => valof{
    try{
      valis _inbytes(H,Cx)
    } catch {
      | .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  public rdBytesAsync:async (ioHandle,integer)=> vect[integer] throws ioException.
  rdBytesAsync(IO,Cx) => valof{
    try{
      valis waitforIO(IO,_inbytes_async(IO,Cx))
    } catch {
      | .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  public rdFile:(string)=> string throws ioException.
  rdFile(F) => valof{
    try{
      valis _get_file(F);
    } catch {
      .eof => throw .pastEof
      | _ default => throw .ioError
    }
  }

  close:(ioHandle)=>() throws ioException.
  close(FH) => valof{
    try{
      valis _close(FH);
    } catch {
      | .eof => throw .pastEof
      | _ => throw .ioError
    }
  }

  openInFile:(string) => ioHandle throws ioException.
  openInFile(Fl) => (try
    _openInFile(Fl,3)
    catch {
      _ => throw .ioError
    }).

  public rdFileAsync:async (string)=> string throws ioException.
  rdFileAsync(Fl) => valof{
    In = openInFile(Fl);
    Txt := [];

    while Ln.=rdCharsAsync(In,1024) do{
      Txt := [Ln,..Txt!];
    }
    close(In);
    valis reverse(Txt!)*
  }

  public wrChar:(ioHandle,char) => () throws ioException.
  wrChar(H,C) => valof{
    try{
      valis _outchar(H,C)
    } catch {
      | _ default => throw .ioError
    }
  }

  public wrCharAsync:async (ioHandle,char)=> () throws ioException.
  wrCharAsync(IO,C) => valof{
    try{
      valis waitforIO(IO,_outchar_async(IO,C))
    } catch {
      | _ default => throw .ioError
    }
  }

  public wrText:(ioHandle,string) => () throws ioException.
  wrText(H,S) => valof{
    try{
      valis _outtext(H,S)
    } catch {
      | _ default => throw .ioError
    }
  }

  public wrTextAsync:async (ioHandle,string)=>() throws ioException.
  wrTextAsync(IO,S) => valof{
    try{
      valis waitforIO(IO,_outtext_async(IO,S))
    } catch {
      | _ default => throw .ioError
    }
  }

  public wrFile:(string,string) => () throws ioException.
  wrFile(F,S) => valof{
    try{
      valis _put_file(F,S)
    } catch {
      | _ default => throw .ioError
    }
  }

  public wrFileAsync:async (string,string)=> () throws ioException.
  wrFileAsync(F,S) => valof{
    try{
      Ot = _openOutFile(F,3);
      wrTextAsync(Ot,S);
      _close(Ot);
      valis ()
    } catch {
      _ default => throw .ioError
    }
  }

  waitforIO:all k,e ~~ async (ioHandle,future[k,e])=>k throws e.
  waitforIO(IO,Ft) => valof{
    case this suspend .requestIO(IO,()=>~_futureIsResolved(Ft)) in {
      | .go_ahead => {
	valis _futureVal(Ft)
      }
      | _ => retire .retired_
    }
  }
  
}
