star.compiler.freshen{
  import star.
  import star.iterable.

  import star.compiler.dict.
  import star.compiler.misc.
  import star.compiler.types.

  public freshen:(tipe,dict) => (cons[(string,tipe)],tipe).
  freshen(Tp,Env) where (T,Q,Ev) .= freshQuants(deRef(Tp),[],Env) =>
    (Q,frshn(deRef(T),Ev)).
  freshen(Tp,_) default => ([],Tp).

  public refresh:(cons[(string,tipe)],tipe,dict) => tipe.
  refresh(Q,T,Env) => frshn(deRef(T),
    foldLeft(((QNm,QTp),E)=>declareType(QNm,.none,QTp,faceType([],[]),E),Env,Q)).

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
  skolQ(Tp,Env) => (Tp,Env).

  freshQuants:(tipe,cons[(string,tipe)],dict)=>(tipe,cons[(string,tipe)],dict).
  freshQuants(allType(nomnal(V),T),B,Env) where NV.=newTypeVar(V) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  freshQuants(allType(kFun(V,Ar),T),B,Env) where NV.=newTypeFun(V,Ar) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  freshQuants(existType(nomnal(V),T),B,Env) where NV.=genSkolemFun(V,B) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,faceType([],[]),Env)).
  freshQuants(existType(V,T),B,Env) =>
    freshQuants(deRef(T),B,Env).
  freshQuants(T,B,Env) default => (T,B,Env).

  genSkolemFun(Nm,[]) => skolemFun(Nm,0).
  genSkolemFun(Nm,Q) => foldLeft(((_,V),S)=>tpExp(S,V),skolemFun(Nm,size(Q)),Q).

  skolemFun:(string,integer) => tipe.
  skolemFun(Nm,0) => nomnal(genSym(Nm)).
  skolemFun(Nm,Ar) => kFun(genSym(Nm),Ar).

  public evidence:(tipe,dict) => (cons[(string,tipe)],tipe).
  evidence(Tp,Env) where (T,Q,Ev).=skolemQuants(deRef(Tp),[],Env) =>
    (Q,frshn(deRef(T),Ev)).
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
  genTypeFun(Nm,Q) => foldLeft(((_,V),S)=>tpExp(S,V),newTypeFun(Nm,size(Q)),Q).

  frshn:(tipe,dict)=>tipe.
  frshn(nomnal(Nm),Env) where (_,Tp,_)^=findType(Env,Nm) => Tp.
  frshn(nomnal(Nm),_) => nomnal(Nm).
  frshn(kFun(Nm,Ar),Env) where  (_,Tp,_)^=findType(Env,Nm) => Tp.
  frshn(kFun(Nm,Ar),_) => kFun(Nm,Ar).
  frshn(tVar(T,N),_) => tVar(T,N).
  frshn(tFun(T,A,N),_) => tFun(T,A,N).
  frshn(tpFun(N,A),_) => tpFun(N,A).
  frshn(tpExp(O,A),Env) => tpExp(rewrite(O,Env),rewrite(A,Env)).
  frshn(tupleType(Els),Env) => tupleType(frshnList(Els,Env)).
  frshn(faceType(Els,Tps),Env) =>
    faceType(Els//(((Nm,E))=>(Nm,rewrite(E,Env))),
      Tps//(((Nm,E))=>(Nm,rewrite(E,Env)))).
  frshn(funDeps(T,D),Env) => funDeps(rewrite(T,Env),
    frshnList(D,Env)).
  frshn(allType(K,T),Env) => allType(K,frshn(T,Env)).
  frshn(existType(K,T),Env) => existType(K,rewrite(T,Env)).
  frshn(typeLambda(H,T),Env) => typeLambda(rewrite(H,Env),rewrite(T,Env)).
  frshn(typeExists(H,T),Env) => typeExists(rewrite(H,Env),rewrite(T,Env)).
  frshn(constrainedType(T,C),Env) => constrainedType(rewrite(T,Env),frshnConstraint(C,Env)).

  frshnList:(cons[tipe],dict) => cons[tipe].
  frshnList(As,Env) => (As//(E)=>frshn(deRef(E),Env)).

  rewrite(Tp,Env) => frshn(deRef(Tp),Env).

  frshnConstraint(contractConstraint(Tp),Env) => contractConstraint(rewrite(Tp,Env)).
  frshnConstraint(fieldConstraint(T,I),Env) =>
    fieldConstraint(rewrite(T,Env),rewrite(I,Env)).

}
