star.io{
  import star.
  import star.mbox.
  import star.file.

  public ioException ::= .badIo | .pastEof.

  public rdChar:raises ioException |: (ioHandle) => char.
  rdChar(H) => valof{
    try{
      valis _inchar(H)
    } catch errorCode in {
      | .eof => raise .pastEof
      | _ default => raise .badIo
    }
  }

  public rdCharAsync:all e ~~ (this:task[e]), raises ioException|:(ioHandle)=> char.
  rdCharAsync(IO) => valof{
    Fut = _inchar_async(IO);
    case this suspend .requestIO(()=>~_futureIsResolved(Fut)) in {
      .go_ahead => {
	if _futureIsResolved(Fut) then{
	  try{
	    valis _futureVal(Fut)
	  } catch errorCode in {
	    | .eof => raise .pastEof
	    | _ default => raise .badIo
	  }
	}
	else
	this retire .retired_
      }
    }
  }
}
