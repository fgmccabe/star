test.proc{
  import star.
  import star.assert.

  -- Test the new procedure notation

  -- A simple procedure type
  do1: (integer){}.
  do1(Ix){
    show "do 1 with $(Ix)"
  }

  -- A procedure type with a throws clause
  do2: (string,integer){} throws exception.
  do2(Msg,Code){
    show "Try to divide";

    do1(Code/0);
  }

  /*
  v1:(string)=>integer.
  v1(S) => valof{
    try{
      do2("there",42) -- should report a syntax error
    } catch {
      _ => valis 42
    }
  }
  */

  v2:(string)=>integer.
  v2(S) => valof{
    try{
      do2("there",42)
    } catch {
      _ do valis 42
    };
    valis 43
  }

  main:(integer){}.
  main(Ix){
    do1(Ix);

    try{
      do2("hello",42);
      do1(43)
    } catch {
      X do logMsg(.info,"$(X)")
    }
  }
}
