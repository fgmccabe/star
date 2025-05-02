test.ac7{
  import star.
  import star.assert.

  f: (integer) => integer throws string.
  f(X) => valof{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX = f(X-1);
      valis XX*X
    } else
    throw "illegal arg"
  }

  main:()=>().
  main() => valof{
    try{
      F10 = f(10);
      show F10;
    } catch {
      E => {
	logMsg(.warning,E)
      }
    };
    valis ()
  }
}
  
