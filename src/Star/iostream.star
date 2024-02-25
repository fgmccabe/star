star.iostream{
  import star.
  import star.io.
  import star.file.

  public all x ~~ inputStream[x] ::=
    .endStream |
    .streamThunk(thunk[inputStream[x]]) |
    .streamPair(x,inputStream[x]).

  public implementation all x ~~ stream[inputStream[x]->>x] => let{.
    eof_(.endStream) => .true.
    eof_(.streamThunk(Th)) => eof_(Th!!).
    eof_(_) default => .false.

    hdtl_(.streamPair(H,T)) => .some((H,T)).
    hdtl_(.streamThunk(Th)) => hdtl_(Th!!).
    hdtl_(_) default => .none
  .} in {
    _eof = eof_.
    _hdtl = hdtl_
  }

  public implementation all e ~~ display[e] |: display[inputStream[e]] => let{.
    strmDisp(.endStream,L) => L.
    strmDisp(.streamPair(X,.endStream),L) => .cons(disp(X), L).
    strmDisp(.streamThunk(_),L) => .cons("thunk", L).
    strmDisp(.streamPair(X,R),L) => .cons(disp(X), .cons(",", strmDisp(R,L))).
 .} in {
    disp(L) => _str_multicat(.cons("[",strmDisp(L,.cons("]",.nil))))
  }

  public implementation all e ~~ folding[inputStream[e]->>e] => let{.
    fold(F,U,.endStream) => U.
    fold(F,U,.streamThunk(Th)) => fold(F,U,Th!!).
    fold(F,U,.streamPair(x,Rst)) => fold(F,F(x,U),Rst).
  .} in {
    foldLeft = fold.
    foldRight = fold.
  }

  public inStream:all t ~~ display[t], raises ioException |:
    (string,(raises ioException|:(ioHandle)=>t))=>inputStream[t].
  inStream(Fl,Fn) => valof{
    try{
      Io = _openInFile(Fl,3);

      let{.
	next() => valof{
	  try{
	    Nxt = Fn(Io);
	    valis .streamPair(Nxt,.streamThunk($$next()))
	  } catch ioException in {
	    _ => {
	      _close(Io);
	      valis .endStream
	    }
	  }
	}
      .} in {
	valis .streamThunk($$next())
      }
    } catch errorCode in {
      _ => raise .ioError
    }
  }

  public inCharStream:raises ioException |: (string) => inputStream[char].
  inCharStream(Fl) => inStream(Fl,(Io)=>rdChar(Io)).

  public inLineStream:raises ioException |: (string) => inputStream[string].
  inLineStream(Fl) => inStream(Fl,(Io)=>rdLine(Io)).

  public inBytesStream:raises ioException |: (string,integer) =>
    inputStream[vect[integer]].
  inBytesStream(Fl,BfSze) => inStream(Fl,(Io)=>rdBytes(Io,BfSze)).

  public forceStream:all e ~~ (inputStream[e]) => cons[e].
  forceStream(.endStream) => .nil.
  forceStream(.streamPair(H,T)) => .cons(H,forceStream(T)).
  forceStream(.streamThunk(Th)) => forceStream(Th!!).
}

  
