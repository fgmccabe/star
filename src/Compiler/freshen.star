star.compiler.freshen{
  import star.
  import star.iterable.

  import star.compiler.dict.
  import star.compiler.misc.
  import star.compiler.types.

  public freshen:(tipe,set[tipe],dict) => (list[(string,tipe)],tipe).
  freshen(Tp,Ex,Env) where (T,Q,Ev) .= freshQuants(deRef(Tp),[],Env) => (Q,frshn(deRef(T),Ex,Ev)).
  freshen(Tp,_,_) default => ([],Tp).

  freshQuants(allType(kVar(V),T),B,Env) where NV.=newTypeVar(V) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,none,NV,Env)).
  freshQuants(allType(kFun(V,Ar),T),B,Env) where NV.=newTypeFun(V,Ar) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,none,NV,Env)).
  freshQuants(existType(kVar(V),T),B,Env) where NV.=genSkolemFun(V,B) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,none,NV,Env)).
  freshQuants(T,B,Env) default => (T,B,Env).

  genSkolemFun(Nm,[]) => skolemFun(Nm,0).
  genSkolemFun(Nm,Q) => foldLeft((S,(_,V))=>tpExp(S,V),skolemFun(Nm,size(Q)),Q).

  public evidence:(tipe,set[tipe],dict) => (list[(string,tipe)],tipe).
  evidence(Tp,Ex,Env) where (T,Q,Ev).=skolemQuants(deRef(Tp),[],Env) => (Q,frshn(deRef(T),Ex,Ev)).
  evidence(Tp,_,_) default => ([],Tp).

  skolemQuants(allType(kVar(V),T),B,Env) where NV.=skolemFun(V,0) =>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,none,NV,Env)).
  skolemQuants(allType(kFun(V,Ar),T),B,Env)  where NV.=skolemFun(V,Ar)=>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,none,NV,Env)).
  skolemQuants(existType(kVar(V),T),B,Env) where NV.=genTypeFun(V,B) =>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,none,NV,Env)).
  skolemQuants(T,B,Env) default => (T,B,Env).

  genTypeFun(Nm,[]) => newTypeVar(Nm).
  genTypeFun(Nm,Q) => foldLeft((S,(_,V))=>tpExp(S,V),newTypeFun(Nm,size(Q)),Q).

  frshn(voidType,_,_) => voidType.
  frshn(kVar(Nm),Ex,_) where _contains(Ex,kVar(Nm)) => kVar(Nm).
  frshn(kVar(Nm),_,Env) where (_,Tp,_)^=findType(Env,Nm) => Tp.
  frshn(kVar(Nm),_,_) => kVar(Nm).

  frshn(kFun(Nm,Ar),Ex,_) where _contains(Ex,kFun(Nm,Ar)) => kFun(Nm,Ar).
  frshn(kFun(Nm,Ar),_,Env) where  (_,Tp,_)^=findType(Env,Nm) => Tp.
  frshn(kFun(Nm,Ar),_,_) => kFun(Nm,Ar).

  frshn(tVar(T,N),_,_) => tVar(T,N).
  frshn(tFun(T,A,N),_,_) => tFun(T,A,N).

  frshn(tipe(N),_,_) => tipe(N).
  frshn(tpFun(N,A),_,_) => tpFun(N,A).
  frshn(tpExp(O,A),Ex,Env) => tpExp(rewrite(O,Ex,Env),rewrite(A,Ex,Env)).
  frshn(tupleType(Els),Ex,Env) => tupleType(Els//((E)=>rewrite(E,Ex,Env))).
  frshn(faceType(Els,Tps),Ex,Env) => faceType(Els//(((Nm,E))=>(Nm,rewrite(E,Ex,Env))),Tps//(((Nm,E))=>(Nm,rewrite(E,Ex,Env)))).
  frshn(allType(K,T),Ex,Env) => allType(K,rewrite(T,_addMem(K,Ex),Env)).
  frshn(existType(K,T),Ex,Env) => existType(K,rewrite(T,_addMem(K,Ex),Env)).
  frshn(typeLambda(H,T),Ex,Env) => typeLambda(rewrite(H,Ex,Env),rewrite(T,Ex,Env)).
  frshn(typeExists(H,T),Ex,Env) => typeExists(rewrite(H,Ex,Env),rewrite(T,Ex,Env)).
  frshn(constrainedType(T,C),Ex,Env) => constrainedType(rewrite(T,Ex,Env),frshnConstraint(C,Ex,Env)).

  rewrite(Tp,Ex,Env) => frshn(deRef(Tp),Ex,Env).

  frshnConstraint(conConstraint(Nm,Args,Deps),Ex,Env) =>
    conConstraint(Nm,Args//((E)=>rewrite(E,Ex,Env)),Deps//((E)=>rewrite(E,Ex,Env))).
  frshnConstraint(fieldConstraint(T,I),Ex,Env) => fieldConstraint(rewrite(T,Ex,Env),rewrite(I,Ex,Env)).

  public freshenContractDefn:(contractDefn,set[tipe],dict) => (list[(string,tipe)],constraint,tipe).
  freshenContractDefn(conDfn(_,_,Q,A,D,I),Ex,Env) =>
    

}
