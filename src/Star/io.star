star.io{
  import star.
  import star.mbox.
  public import star.file.

  public inHandle ::= .inHandle(ioHandle).

  public outHandle ::= .outHandle(ioHandle).

  public atEof:(inHandle) => boolean.
  atEof(.inHandle(I)) => _end_of_file(I).

  public rdChar:(inHandle) => char throws ioException.
  rdChar(.inHandle(H)) => valof{
    try{
      valis _inchar(H)
    } catch {
      | .eEOF do throw .pastEof
      | _ do throw .ioError
    }
  }

  public rdCharAsync:async (inHandle)=>char throws ioException.
  rdCharAsync(.inHandle(I)) => valof{
    try{
      valis waitforIO(I,_inchar_async(I))
    } catch {
      | .eEOF do throw .pastEof
      | _ do throw .ioError
    }
  }

  rdCharsAsync:async (inHandle,integer)=>string throws ioException.
  rdCharsAsync(.inHandle(I),Cx) => valof{
    try{
      valis waitforIO(I,_inchars_async(I,Cx))
    } catch {
      | .eEOF do throw .pastEof
      | _ do throw .ioError
    }
  }

  public rdLine:(inHandle) => string throws ioException.
  rdLine(.inHandle(I)) => valof{
    try{
      valis _inline(I)
    } catch {
      | .eEOF do throw .pastEof
      | _ do throw .ioError
    }
  }

  public rdLineAsync:async (inHandle)=> string throws ioException.
  rdLineAsync(.inHandle(I)) => valof{
    try{
      valis waitforIO(I,_inline_async(I))
    } catch {
      | .eEOF do throw .pastEof
      | _ do throw .ioError
    }
  }

  public rdBytes:(inHandle,integer) => vect[integer] throws ioException.
  rdBytes(.inHandle(H),Cx) => valof{
    try{
      valis _inbytes(H,Cx)
    } catch {
      | .eEOF do throw .pastEof
      | _ do throw .ioError
    }
  }

  public rdBytesAsync:async (inHandle,integer)=> vect[integer] throws ioException.
  rdBytesAsync(.inHandle(I),Cx) => valof{
    try{
      valis waitforIO(I,_inbytes_async(I,Cx))
    } catch {
      | .eEOF do throw .pastEof
      | _ do throw .ioError
    }
  }

  public rdFile:(string, textEncoding)=> string throws ioException.
  rdFile(Fn,Enc) => valof{
    try{
      valis _get_file(Fn,pickEncoding(Enc));
    } catch {
      | .eNOTFND do throw .notFound
      | .eNOPERM do throw .noPerm
      | _ do throw .ioError
    }
  }

  public contract all i,e ~~ closer[i->>e] ::= {
    close:(i){} throws e
  }

  public implementation closer[inHandle->>ioException] => {
    close(.inHandle(I)) {
      try{
	_close(I)
      } catch {
	_ do throw .ioError
      }
    }
  }

  public implementation closer[outHandle->>ioException] => {
    close(.outHandle(O)) {
      try{
	_close(O)
      } catch {
	_ do throw .ioError
      }
    }
  }

  public openInFile:(string,textEncoding) => inHandle throws ioException.
  openInFile(Fn,Enc) => valof{
    try{
      valis .inHandle(_openInFile(Fn,pickEncoding(Enc)))
    } catch {
      _ do throw .notFound
    }
  }

  public rdFileAsync:async (string)=> string throws ioException.
  rdFileAsync(Fl) => valof{
    In = openInFile(Fl,.utf8Encoding);
    Txt := [];

    while Ln.=rdCharsAsync(In,1024) do{
      Txt := [Ln,..Txt!];
    }
    close(In);
    valis reverse(Txt!)*
  }

  public wrChar:(outHandle,char){} throws ioException.
  wrChar(.outHandle(O),C){
    try{
      _outchar(O,C)
    } catch {
      | _ do throw .ioError
    }
  }

  public wrCharAsync:async (outHandle,char){} throws ioException.
  wrCharAsync(.outHandle(O),C){
    try{
      waitforIO(O,_outchar_async(O,C))
    } catch {
      | _ do throw .ioError
    }
  }

  public wrText:(outHandle,string){} throws ioException.
  wrText(.outHandle(O),S){
    try{
      _outtext(O,S)
    } catch {
      | _ do throw .ioError
    }
  }

  public wrTextAsync:async (outHandle,string){} throws ioException.
  wrTextAsync(.outHandle(O),S){
    try{
      waitforIO(O,_outtext_async(O,S))
    } catch {
      | _ do throw .ioError
    }
  }

  public wrFile:(string,textEncoding,string){} throws ioException.
  wrFile(F,Enc,S){
    try{
      _put_file(F,pickEncoding(Enc),S)
    } catch {
      | .eNOPERM do throw .noPerm
      | _ do throw .ioError
    }
  }

  public openOutFile:(string,textEncoding) => outHandle throws ioException.
  openOutFile(Fl,E) => (try
    .outHandle(_openOutFile(Fl,pickEncoding(E)))
    catch {
      _ => throw .ioError
    }).

  public wrFileAsync:async (string,string){} throws ioException.
  wrFileAsync(F,S){
    Ot = openOutFile(F,.utf8Encoding);
    wrTextAsync(Ot,S);
    close(Ot)
  }

  waitforIO:all k,e ~~ async (ioHandle,future[k,e])=>k throws e.
  waitforIO(IO,Ft) => valof{
    case this suspend .requestIO(IO,()=>~_futureIsResolved(Ft)) in {
      | .go_ahead do {
	_futureVal(Ft)
      }
      | _ do retire .retired_
    }
  }

  public stdin:inHandle.
  stdin = .inHandle(_stdfile(0)).

  public stdout:outHandle.
  stdout = .outHandle(_stdfile(1)).

  public stderr:outHandle.
  stderr = .outHandle(_stdfile(2)).
}
