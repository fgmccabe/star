test.ac7{
  import star.

  f:(integer) => integer throws string.
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
      _logmsg(disp(F10));
    } catch {
      E => {
	_logmsg(E)
      }
    };
    valis ()
  }
}
  
