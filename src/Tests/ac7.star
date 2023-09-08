test.ac7{
  import star.

  f: raises string |: (integer) => integer.
  f(X) => valof{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX = f(X-1);
      valis XX*X
    } else
    raise "illegal arg"
  }

  main:()=>().
  main() => valof{
    try{
      F10 = f(10);
      logMsg(disp(F10));
    } catch string in {
      E => {
	logMsg(E)
      }
    };
    valis ()
  }
}
  
