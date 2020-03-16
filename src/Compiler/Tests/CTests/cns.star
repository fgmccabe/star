test.cns{
  import star.core.
  import test.ar.

  public cons[x] ::= .nil | cons(x,cons[x]).

  public len:all a~~(cons[a])=>integer.
  len(.nil)=>0.
  len(cons(_,L))=>len(L)+1.

  public nth:all a~~(cons[a],integer)=>a.
  nth(cons(E,_),0) => E.
  nth(cons(_,L),Ix) => nth(L,Ix-1).
}
