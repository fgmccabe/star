test.y{
  import star.
  import star.assert.

  -- Test out some try catch handling

  -- A function that throws an exception
  fooE:() => string throws exception.
  fooE() => valof{
    showMsg("raising from fooE");
    throw .exception("fooE")
  }

  -- A function that throws a generic exception
  fooG:all e ~~ (()=>e)=>() throws e.
  fooG(E) => throw E().

  -- A function that catches a generic exception
  fooC:all e ~~ (()=>string throws e) => string.
  fooC(F) => 
    (try
      F()
      catch {
	_ => valof{
	  showMsg("we got an exception");
	  valis "we got an exception"
	}
      }).

  main:()=>().
  main() => valof{
    try{
      show fooE()
    } catch {
      .exception(M) => {
	showMsg("fooE threw #(M)")
      }
    };

    try{
      show fooG(()=>"test string")
    } catch {
      M => showMsg("fooG throws $(M)")
    };

    show fooC((()=>"world"):(()=>string throws ()));

    try{
      show fooG(()=>42)
    } catch {
      M => showMsg("fooG throws $(M)")
    };

    valis ()
  }
}
