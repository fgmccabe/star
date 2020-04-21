star.compiler.constraints{
  import star.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.types.
  import star.compiler.unify.

  import star.topsort.

  constraint ::= eqCon(locn,tipe,tipe) |
    conCon(locn,string,cons[tipe]) |
    andCon(locn,flat,flat) |
    implCon(locn,cons[tipe],constraint,constraint) |
    emptyCon.

  discharge(eqCon(Lc,T1,T2),Env) where
      tpExp(O1,A1) ^= deRef(T1,Env) && tpExp(O2,A2) ^= deRef(T2,Env) => 

  

}
  
