star.inchnnl{
  import star.
  import star.io.
  import star.file.

  public inChnnl ::= .inChnnl(ioHandle,integer).

  public implementation stream[inChnnl->>char] => let{.
    sync(Io,Pos) => valof{
      try{
	if _fposition(Io)~=Pos then{
	  _fseek(Io,Pos)
	}
      } catch errorCode in {
	_ => {}
      };
      valis ()
    }
    
    endOfFile(.inChnnl(Io,Pos)) => valof{
      sync(Io,Pos);
      valis _end_of_file(Io)
    }

    nextChar(.inChnnl(Io,Pos)) => valof{
      sync(Io,Pos);
      try{
	Ch = _inchar(Io);
	valis .some((Ch,.inChnnl(Io,_fposition(Io))))
      } catch errorCode in {
	_ => valis .none
      }
    }
  .} in {
    _eof = endOfFile.
    _hdtl = nextChar
  }

  public implementation display[inChnnl] => {
    disp(.inChnnl(Io,Pos)) => "<In:#(_fname(Io))@$(Pos)>"
  }

  public inChannel:raises errorCode|:(string)=>inChnnl.
  inChannel(Fl) => .inChnnl(_openInFile(Fl,3),0).
}



      
