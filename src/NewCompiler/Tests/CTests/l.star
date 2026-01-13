test.l{
  import star.core.
  import test.ar.
  import test.cns.

  reducer:all x,y ~~ ((x,y)=>y)=>(cons[x],y)=>y.
  reducer(F) => let{.
    rdr(.nil,z) => z.
    rdr(cons(x,l),z) => F(x,rdr(l,z)).
  } in rdr.
  
  foldRight:all x,y ~~ ((x,y)=>y,y,cons[x])=>y.
  foldRight(F,Z,L) => let{.
    Mx = len(L).
    fdr(Ix) where Ix==Mx => Z.
    fdr(Ix) => F(nth(L,Ix),fdr(Ix+1)).
  } in fdr(0).
  
}
