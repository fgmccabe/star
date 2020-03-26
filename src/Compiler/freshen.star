star.compiler.freshen{
  import star.
  import star.iterable.

  import star.compiler.dict.
  import star.compiler.misc.
  import star.compiler.types.

  public freshen:(tipe,dict) => (cons[(string,tipe)],tipe).
  freshen(Tp,Env) where (T,Q,Ev) .= freshQuants(deRef(Tp),[],Env) =>
    (Q,frshn(deRef(T),Ev,skolQ,freshQ)).
  freshen(Tp,_) default => ([],Tp).

  freshQ:(tipe,dict) => (tipe,dict).
  freshQ(nomnal(V),Env) where NV.=newTypeVar(V) =>
    (NV,declareType(V,.none,NV,faceType([],[]),Env)).
  freshQ(kFun(V,Ar),Env) where NV.=newTypeFun(V,Ar) =>
    (NV,declareType(V,.none,NV,faceType([],[]),Env)).

  skolQ:(tipe,dict) => (tipe,dict).
  skolQ(nomnal(V),Env) where NV.=skolemFun(V,0) =>
    (NV,declareType(V,.none,NV,faceType([],[]),Env)).
  skolQ(kFun(V,Ar),Env) where NV.=skolemFun(V,Ar) =>
    (NV,declareType(V,.none,NV,faceType([],[]),Env)).

  freshQuants:(tipe,cons[(string,tipe)],dict)=>(tipe,cons[(string,tipe)],dict).
  freshQuants(allType(nomnal(V),T),B,Env) where NV.=newTypeVar(V) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  freshQuants(allType(kFun(V,Ar),T),B,Env) where NV.=newTypeFun(V,Ar) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  freshQuants(existType(nomnal(V),T),B,Env) where NV.=genSkolemFun(V,B) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  freshQuants(T,B,Env) default => (T,B,Env).

  genSkolemFun(Nm,[]) => skolemFun(Nm,0).
  genSkolemFun(Nm,Q) => foldLeft((S,(_,V))=>tpExp(S,V),skolemFun(Nm,size(Q)),Q).

  skolemFun:(string,integer) => tipe.
  skolemFun(Nm,0) => nomnal(genSym(Nm)).
  skolemFun(Nm,Ar) => kFun(genSym(Nm),Ar).

  public evidence:(tipe,dict) => (cons[(string,tipe)],tipe).
  evidence(Tp,Env) where (T,Q,Ev).=skolemQuants(deRef(Tp),[],Env) =>
    (Q,frshn(deRef(T),Ev,freshQ,skolQ)).
  evidence(Tp,_) default => ([],Tp).

  skolemQuants(allType(nomnal(V),T),B,Env) where .none.=findType(Env,V) =>
    skolemQuants(deRef(T),[(V,nomnal(V)),..B],Env).
  skolemQuants(allType(nomnal(V),T),B,Env) where NV.=skolemFun(V,0) =>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  skolemQuants(allType(kFun(V,Ar),T),B,Env)  where .none.=findType(Env,V) =>
    skolemQuants(deRef(T),[(V,kFun(V,Ar)),..B],declareType(V,.none,kFun(V,Ar),faceType([],[]),Env)).
  skolemQuants(allType(kFun(V,Ar),T),B,Env)  where NV.=skolemFun(V,Ar)=>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  skolemQuants(existType(nomnal(V),T),B,Env) where NV.=genTypeFun(V,B) =>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  skolemQuants(T,B,Env) default => (T,B,Env).

  genTypeFun(Nm,[]) => newTypeVar(Nm).
  genTypeFun(Nm,Q) => foldLeft((S,(_,V))=>tpExp(S,V),newTypeFun(Nm,size(Q)),Q).

  frshn:(tipe,dict,(tipe,dict)=>(tipe,dict),(tipe,dict)=>(tipe,dict))=>tipe.
  frshn(nomnal(Nm),Env,_,_) where (_,Tp,_)^=findType(Env,Nm) => Tp.
  frshn(nomnal(Nm),_,_,_) => nomnal(Nm).
  frshn(kFun(Nm,Ar),Env,_,_) where  (_,Tp,_)^=findType(Env,Nm) => Tp.
  frshn(kFun(Nm,Ar),_,_,_) => kFun(Nm,Ar).
  frshn(tVar(T,N),_,_,_) => tVar(T,N).
  frshn(tFun(T,A,N),_,_,_) => tFun(T,A,N).
  frshn(tpFun(N,A),_,_,_) => tpFun(N,A).
  frshn(tpExp(O,A),Env,F,G) => tpExp(rewrite(O,Env,F,G),rewrite(A,Env,F,G)).
  frshn(tupleType(Els),Env,F,G) => tupleType(frshnList(Els,Env,F,G)).
  frshn(faceType(Els,Tps),Env,F,G) =>
    faceType(Els//(((Nm,E))=>(Nm,rewrite(E,Env,F,G))),
      Tps//(((Nm,E))=>(Nm,rewrite(E,Env,F,G)))).
  frshn(funDeps(T,D),Env,F,G) => funDeps(rewrite(T,Env,F,G),
    frshnList(D,Env,F,G)).
  frshn(allType(K,T),Env,F,G) where (K1,E0).=F(K,Env) => allType(K1,frshn(T,E0,F,G)).
  frshn(existType(K,T),Env,F,G) where (K1,E0) .= G(K,Env) =>
    existType(K1,rewrite(T,Env,F,G)).
  frshn(typeLambda(H,T),Env,F,G) => typeLambda(rewrite(H,Env,F,G),rewrite(T,Env,F,G)).
  frshn(typeExists(H,T),Env,F,G) => typeExists(rewrite(H,Env,F,G),rewrite(T,Env,F,G)).
  frshn(constrainedType(T,C),Env,F,G) => constrainedType(rewrite(T,Env,F,G),frshnConstraint(C,Env,F,G)).

  frshnList:(cons[tipe],dict,(tipe,dict)=>(tipe,dict),(tipe,dict)=>(tipe,dict)) => cons[tipe].
  frshnList(As,Env,F,G) => (As//(E)=>frshn(deRef(E),Env,F,G)).

  rewrite(Tp,Env,F,G) => frshn(deRef(Tp),Env,F,G).

  frshnConstraint(typeConstraint(Tp),Env,F,G) => typeConstraint(rewrite(Tp,Env,F,G)).
  frshnConstraint(fieldConstraint(T,I),Env,F,G) =>
    fieldConstraint(rewrite(T,Env,F,G),rewrite(I,Env,F,G)).

  public rewriteType:(tipe,map[tipe,tipe])=>tipe.
  rewriteType(Tp,Env) => rewr(deRef(Tp),Env).
  
  rewr(kFun(Nm,Ar),Env) where T^=Env[kFun(Nm,Ar)] => T.
  rewr(nomnal(Nm),Env) where T^=Env[nomnal(Nm)] => T.
  rewr(V,_) where isUnbound(V) => V.
  rewr(kFun(Nm,Ar),Env) => kFun(Nm,Ar).
  rewr(nomnal(Nm),Env) => nomnal(Nm).
  rewr(tpFun(Nm,Ar),_) => tpFun(Nm,Ar).
  rewr(tpExp(Op,A),Env) => tpExp(rewriteType(Op,Env),rewriteType(A,Env)).
  rewr(tupleType(Els),Env) => tupleType(Els//(E)=>rewriteType(E,Env)).
  rewr(allType(V,B),Env) => _ ^= Env[V] ? allType(V,B) || allType(V,rewriteType(B,Env)).
  rewr(existType(V,B),Env) => _ ^= Env[V] ? existType(V,B) || existType(V,rewriteType(B,Env)).
  rewr(faceType(Flds,Tps),Env) => faceType(Flds//((Nm,T))=>(Nm,rewriteType(T,Env)),
    Tps//((Nm,T))=>(Nm,rewriteType(T,Env))).
  rewr(typeLambda(A,R),_) => typeLambda(A,R). -- fix me
  rewr(typeExists(A,R),_) => typeExists(A,R). -- me too
  rewr(constrainedType(T,C),Env) => constrainedType(rewriteType(T,Env),rewriteCon(C,Env)).
  rewr(funDeps(T,Tps),Env) => funDeps(rewriteType(T,Env),Tps//(E)=>rewriteType(E,Env)).

  rewriteCon(typeConstraint(T),Env) => typeConstraint(rewriteType(T,Env)).
  rewriteCon(fieldConstraint(F,T),Env) => fieldConstraint(rewriteType(F,Env),rewriteType(T,Env)).
}
