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
    fold:all u ~~ ((e,u)=>u,u,inputStream[e])=>u.
    fold(F,U,.endStream) => U.
    fold(F,U,.streamThunk(Th)) => fold(F,U,Th!!).
    fold(F,U,.streamPair(x,Rst)) => fold(F,F(x,U),Rst).
  .} in {
    foldLeft = fold.
    foldRight = fold.
  }

  public inStream:all t ~~ display[t] |:
    (string,((ioHandle)=>t throws ioException))=>inputStream[t] throws ioException.
  inStream(Fl,Fn) => valof{
    Io = openInFile(Fl);

    let{.
      next() => valof{
	try{
	  Nxt = Fn(Io);
	  valis .streamPair(Nxt,.streamThunk($$next()))
	} catch {
	  _ => {
	    close(Io);
	    valis .endStream
	  }
	}
      }
    .} in {
      valis .streamThunk($$next())
    }
  }

  public inCharStream:(string) => inputStream[char] throws ioException.
  inCharStream(Fl) => inStream(Fl,(Io)=>rdChar(Io)).

  public inLineStream:(string) => inputStream[string] throws ioException.
  inLineStream(Fl) => inStream(Fl,(Io)=>rdLine(Io)).

  public inBytesStream: (string,integer) =>
    inputStream[vect[integer]] throws ioException.
  inBytesStream(Fl,BfSze) => inStream(Fl,(Io)=>rdBytes(Io,BfSze)).

  public forceStream:all e ~~ (inputStream[e]) => cons[e].
  forceStream(.endStream) => .nil.
  forceStream(.streamPair(H,T)) => .cons(H,forceStream(T)).
  forceStream(.streamThunk(Th)) => forceStream(Th!!).
}

  
