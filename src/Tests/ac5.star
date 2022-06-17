test.ac5{
  import star.core.
  import star.arith.
  import star.action.
  import star.iterable.

  -- test for loops

  all a ~~ range[a]::=range(a,a).

  implementation all a ~~ arith[a],equality[a] |: iter[range[a]->>a] => {.
    _iter(range(X,X),St,_) => St.
    _iter(range(X,Y),St,Fn) => _iter(range(X+1,Y,S),Fn(X,St),Fn)
  .}

  genIter:all c,a ~~ iter[c->>a] |:
    (c) => task[
  
  f:(integer) => result[string,integer].
  f(X) => do{
    if X == 1 then{
      valis 1
    } else if X>1 then{
      XX <- f(X-1);
      valis XX*X
    } else
    raise "illegal arg"
  }

  main:()=>().
  main() => valof{
    _ .= _logmsg(disp(f(10)));
    try{
      F10 <- f(10);
      _ .= _logmsg(disp(F10));
      _ .= _logmsg(disp(f(-10)));
      F <- f(-10);
      _ .= _logmsg(disp(F))
    } catch {
      E => {
	_ .= _logmsg(E)
      }
    };
    valis ()
  }
  
}
