test.lib.text{
  import star.
  import star.io.
  import star.mbox.

  public readAsLines:async (string) => future[cons[string],ioException].
  readAsLines(Fl) => tsk(this,() => valof{
      In = openInFile(Fl);
      out := [];

      while Ln.=rdLineAsync(In) do{
	out := [Ln,..out!]
      };

      close(In);

      valis reverse(out!)
    }).

  public readLines:(string) => cons[string] throws ioException.
  readLines(Fl) => valof{
    In = openInFile(Fl);
    out := [];

    try{
      while Ln.=rdLine(In) do{
	out := [Ln,..out!]
      }
    } catch {
      .pastEof => {}
    };

    close(In);

    valis reverse(out!)
  }.
  
}
