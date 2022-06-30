test.let{
  import star.
  import star.script.

  kk:all x ~~ (x)=> (()=>x).
  kk(X) => let{
    f:()=>x.
    f()=>X.
  } in f.

  inc:(integer) => ((integer)=>integer).
  inc(A) => let{
    pl:(integer)=>integer.
    pl(X) => X+A.
  } in pl.

  fg:()=>(integer)=>integer.
  fg()=>let{.
    f(0)=>1.
    f(N)=>g(N-1)*N.

    g(0)=>1.
    g(N)=>f(N-1)*N
  .} in g.

  main:()=> ().
  main()=> valof{
    assert inc(2)(3) == 5 && k.=kk(3) && k() == 3;

    assert fg()(3) == 6;
    valis ()
  }
}
