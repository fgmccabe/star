test.jt12{
  import star.
  import star.assert.

  -- Jitting gc test

  iota:(integer) => cons[integer].
  iota(M) => let{.
    io(N) where ieq(N,M) => .nil.
    io(N) where ilt(N,M) => .cons(N,io(iadd(N,1))).
  .} in io(0).

  showCons:(cons[integer])=>string.
  showCons(Ls) => let{.
    consDisp(.nil,L) => L.
    consDisp(.cons(X,.nil),L) => .cons(iShow(X), L).
    consDisp(.cons(X,R),L) => .cons(iShow(X), .cons(",", consDisp(R,L))).
  .} in 
  _str_multicat(.cons("[",consDisp(Ls,.cons("]",.nil)))).

  main:() => ().
  main() => valof{
    logM(showCons(iota(10)));

    try{
      _jit_compile("#(__pkg__)@showCons",1);
      _jit_compile("#(__pkg__)@Γ_1@consDisp",3);
      _jit_compile("#(__pkg__)@iShow",1);
      _jit_compile("#(__pkg__)@iota",1);
      _jit_compile("#(__pkg__)@Γ_0@io",2);
      _jit_compile("#(__pkg__)@ieq",2);
      _jit_compile("#(__pkg__)@ilt",2);
      _jit_compile("#(__pkg__)@iadd",2);
      _jit_compile("#(__pkg__)@logM",1);
    } catch {
      X => logM(_stringOf(X,0))
    };

    logM(showCons(iota(10)));
    assert showCons(iota(10)) == "[0,1,2,3,4,5,6,7,8,9]"
  }

  logM:(string)=>().
  logM(M) => valof{
    try{
      _logmsg(M)
    } catch {_ => {}};
    valis ()
  }

  ieq:(integer,integer) => boolean.
  ieq(X,Y) => _int_eq(X,Y).

  ilt:(integer,integer) => boolean.
  ilt(X,Y) => _int_lt(X,Y).

  iadd:(integer,integer)=>integer.
  iadd(X,Y) => _int_plus(X,Y).

  iShow:(integer)=>string.
  iShow(Ix) => _int2str(Ix).
  
}
