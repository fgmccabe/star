test.l2{
  import star.core.
  import test.lst.

  fmap:all a,b ~~ ((a)=>b,list[a])=>list[b].
  fmap(_,[]) => [].
  fmap(F,[E,..Els]) => [F(E),..fmap(F,Els)].
}
