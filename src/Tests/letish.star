test.let{
  import star.

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

  assert inc(2)(3) == 5 && k .= kk(3) && k() == 3.
}
