star.iostream{
  import star.
  import star.io.
  import star.file.

  public all x ~~ ioStream[x] ::=
    .endStream |
    .streamThunk(thunk[ioStream[x]]) |
    .streamPair(x,ioStream[x]).

  public implementation all x ~~ stream[ioStream[x]->>x] => let{.
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

  public implementation all e ~~ display[e] |: display[ioStream[e]] => let{.
    strmDisp(.endStream,L) => L.
    strmDisp(.streamPair(X,.endStream),L) => .cons(disp(X), L).
    strmDisp(.streamThunk(_),L) => .cons("thunk", L).
    strmDisp(.streamPair(X,R),L) => .cons(disp(X), .cons(",", strmDisp(R,L))).
 .} in {
    disp(L) => _str_multicat(.cons("[",strmDisp(L,.cons("]",.nil))))
  }

  public inStream:(ioHandle)=>ioStream[char].
  inStream(Io) => let{.
    nextChar() => valof{
      try{
	Nxt = rdChar(Io);
	valis .streamPair(Nxt,.streamThunk($$nextChar()))
      } catch ioException in {
	_ => valis .endStream
      }
    }
  .} in .streamThunk($$nextChar()).

  public forceStream:all e ~~ (ioStream[e]) => cons[e].
  forceStream(.endStream) => .nil.
  forceStream(.streamPair(H,T)) => .cons(H,forceStream(T)).
  forceStream(.streamThunk(Th)) => forceStream(Th!!).
}

  
