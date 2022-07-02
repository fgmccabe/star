test.ac7{
  import star.

  f:(integer) => result[string,integer].
  f(X) => do{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX .= valof f(X-1);
      valis XX*X
    } else
    raise "illegal arg"
  }

  main:()=>().
  main() => valof{
    try{
      F10 .= valof f(10);
      _logmsg(disp(F10));
    } catch {
      E => {
	_ .= _logmsg(E)
      }
    };
    valis ()
  }
}
  
