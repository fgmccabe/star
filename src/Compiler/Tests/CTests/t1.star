test.t1{
  import star.core.
  import test.ar.
  import test.cns.

  ff:all v ~~ (v,cons[v])=>cons[v].
  ff(E,L)=>let{
    fff(.nil,_)=>cons(E,.nil).
    fff(cons(H,T),D)=>ptch(T,(El)=>fff(El,D)).
  } in fff(L,0).

  ptch:all x ~~ (cons[x],(cons[x])=>cons[x])=>cons[x].
  ptch(.nil,F)=>F(.nil).
}
