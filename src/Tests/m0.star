test.m0{
  -- Test let environments
  -- Mutually recursive version of factorial

  fg(X) => let{.
    f(0) => 1.
    f(A) => _int_times(G(_int_minus(A,1)),A).

    g(0) => 1.
    g(A) => _int_times(f(_int_minus(A,1)),A).

    F = f.
    G = g.

    h = F(X).

  .} in h.

  private assrt(Tst,Msg) => valof{
    if ~Tst then{
      _logmsg("failed assert #(Msg)");
      _exit(1)
    };
    valis ()
  }

  main:(){}.
  main(){
    _logmsg(_stringOf(fg(5),0));
    assrt(120.=fg(5),"fg(5)==120")
  }
}
