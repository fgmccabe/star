test.jt12{
  -- Jitting test of fibers

  fbr0:(integer)=>fiber[integer,char].
  rbr0(X) => _fiber( (Gen,_) => (Gen retire X)).

  run0:() => integer.
  run() => valof{
    F = fbr0(42);
    valis F resume ` `
  }

  main:() => ().
  main() => valof{
    logM(_int2str(run0()));
    assert run0() == 42;

    try{
      _jit_compile("#(__pkg__)@rfb0",0);
      _jit_compile("#(__pkg__)@run0",0);
      _jit_compile("#(__pkg__)@conc",2);
      _jit_compile("#(__pkg__)@logM",1);
      _jit_compile("#(__pkg__)λ_0",3);
    } catch {
      X => logM(_stringOf(X,0))
    };

    logM(_int2str(run0()));
    assert run0() == 42;
  }

  conc:(string,string)=>string.
  conc(A,B) => _str_concat(A,B).

  logM:(string)=>().
  logM(M) => valof{
    try{
      _logmsg(M)
    } catch {_ => {}};
    valis ()
  }
  
}
