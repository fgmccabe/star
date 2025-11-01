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
    show "Try to divide: $(Code/0)";

    do1(Code);

    throw .exception(Msg)
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
      _ => valis 42
    };
    valis 43
  }

  main:(){}.
  main(){
    do1(23);

    try{
      do2("hello",42);
      do1(43)
    } catch {
      X => logMsg(.info,"$(X)")
    }
  }
}
