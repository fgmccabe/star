star.compiler.freshen{
  import star.
  import star.iterable.

  import star.compiler.dict.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.types.

  public contract all t ~~ fresh[t] ::= {
    freshen:(t,dict) => (cons[(string,tipe)],t).
    refresh:(cons[(string,tipe)],t,dict) => t.
  }

  public implementation fresh[tipe] => {
    freshen(Tp,Env) where (T,Q,Ev) .= freshQuants(deRef(Tp),[],Env) =>
      (Q,frshn(deRef(T),declareTypeVars(Q,Ev))).
    freshen(Tp,_) default => ([],Tp).

    refresh(Q,T,Env) => frshn(deRef(T),declareTypeVars(Q,Env)).
  }

  public implementation fresh[typeRule] => let{
    freshRule(.typeExists(H,T),E) =>
      .typeExists(frshn(deRef(H),E),frshn(deRef(T),E)).
    freshRule(.contractExists(N,T,D,F),E) =>
      .contractExists(N,frshnList(T,E),frshnList(D,E),frshn(deRef(F),E)).
    freshRule(.typeLambda(H,T),E) =>
      .typeLambda(frshn(deRef(H),E),frshn(deRef(T),E)).
  } in {
    freshen(Rl,Env) where (Q,R,E) .= genQuants(Rl,[],Env) => (Q,freshRule(R,E)).
    refresh(Q,Rl,Env) =>
      freshRule(Rl,declareTypeVars(Q,Env)).
  }

  freshenRl(Rl,Env) => valof{
    (_,RRl) = freshen(Rl,Env);
    valis RRl
  }
  
  genQuants:(typeRule,cons[(string,tipe)],dict)=>(cons[(string,tipe)],typeRule,dict).
  genQuants(.allRule(.kVar(V),R),Q,E) => valof{
    NV = newTypeVar(V);
    valis genQuants(R,[(V,NV),..Q],declareType(V,.none,NV,.typeExists(NV,emptyFace),E))
  }
  genQuants(.allRule(.kFun(V,Ar),R),Q,E) => valof{
    NV = newTypeFun(V,Ar);
    valis genQuants(R,[(V,NV),..Q],declareType(V,.none,NV,.typeExists(NV,emptyFace),E))
  }
  genQuants(R,Q,E) => (Q,R,E).

  freshQ:(tipe,dict) => (tipe,dict).
  freshQ(.kVar(V),Env) where NV.=newTypeVar(V) =>
    (NV,declareType(V,.none,NV,.typeExists(NV,emptyFace),Env)).
  freshQ(.kFun(V,Ar),Env) where NV.=newTypeFun(V,Ar) =>
    (NV,declareType(V,.none,NV,.typeExists(NV,emptyFace),Env)).

  skolQ:(tipe,dict) => (tipe,dict).
  skolQ(.kVar(V),Env) where NV.=skolemFun(V,0) =>
    (NV,declareType(V,.none,NV,.typeExists(NV,emptyFace),Env)).
  skolQ(.kFun(V,Ar),Env) where NV.=skolemFun(V,Ar) =>
    (NV,declareType(V,.none,NV,.typeExists(NV,emptyFace),Env)).
  skolQ(Tp,Env) => (Tp,Env).

  freshQuants:(tipe,cons[(string,tipe)],dict)=>(tipe,cons[(string,tipe)],dict).
  freshQuants(.allType(.kVar(V),T),B,Env) where NV.=newTypeVar(V) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
      .typeExists(NV,emptyFace),Env)).
  freshQuants(.allType(.kFun(V,Ar),T),B,Env) where NV.=newTypeFun(V,Ar) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,emptyFace),Env)).
  freshQuants(.existType(.kVar(V),T),B,Env) where NV.=genSkolemFun(V,B) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,emptyFace),Env)).
  freshQuants(.existType(V,T),B,Env) =>
    freshQuants(deRef(T),B,Env).
  freshQuants(T,B,Env) default => (T,B,Env).

  genSkolemFun(Nm,_) => skolemFun(Nm,0).
--  genSkolemFun(Nm,Q) => foldLeft(((_,V),S)=>.tpExp(S,V),skolemFun(Nm,size(Q)),Q).

  skolemFun:(string,integer) => tipe.
  skolemFun(Nm,_) => .kVar(genSym(Nm)).
--  skolemFun(Nm,Ar) => .kFun(genSym(Nm),Ar).

  public evidence:(tipe,dict) => (cons[(string,tipe)],tipe).
  evidence(Tp,Env) where (T,Q,Ev).= skolemQuants(deRef(Tp),[],Env) =>
    (Q,frshn(deRef(T),Ev)).
  evidence(Tp,_) default => ([],Tp).

  skolemQuants(.allType(.kVar(V),T),B,Env) where .none.=findType(Env,V) =>
    skolemQuants(deRef(T),[(V,.kVar(V)),..B],Env).
  skolemQuants(.allType(.kVar(V),T),B,Env) where NV.=skolemFun(V,0) =>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,emptyFace),Env)).
  skolemQuants(.allType(.kFun(V,Ar),T),B,Env)  where .none.=findType(Env,V) =>
    skolemQuants(deRef(T),[(V,.kFun(V,Ar)),..B],declareType(V,.none,.kFun(V,Ar),
	.typeExists(.kFun(V,Ar),emptyFace),Env)).
  skolemQuants(.allType(.kFun(V,Ar),T),B,Env)  where NV.=skolemFun(V,Ar)=>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,emptyFace),Env)).
  skolemQuants(.existType(.kVar(V),T),B,Env) where NV.=genTypeFun(V,B) =>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,emptyFace),Env)).
  skolemQuants(T,B,Env) default => (T,B,Env).

  genTypeFun(Nm,[]) => newTypeVar(Nm).
  genTypeFun(Nm,Q) => foldLeft(((_,V),S)=>.tpExp(S,V),newTypeFun(Nm,size(Q)),Q).

  frshn:(tipe,dict)=>tipe.
  frshn(.anonType,_) => newTypeVar("_").
  frshn(.voidType,_) => .voidType.
  frshn(.nomnal("_"),_) => newTypeVar("_").
  frshn(.kVar(Nm),Env) where (_,Tp,_,_)?=findType(Env,Nm) => Tp.
  frshn(.kVar(Nm),_) => .kVar(Nm).
  frshn(.kFun(Nm,Ar),Env) where  (_,Tp,_,_)?=findType(Env,Nm) => Tp.
  frshn(.kFun(Nm,Ar),_) => .kFun(Nm,Ar).
  frshn(.tVar(T,N),_) => .tVar(T,N).
  frshn(.nomnal(Nm),Env) where (_,Tp,_,_)?=findType(Env,Nm) => Tp.
  frshn(.nomnal(Nm),_) => .nomnal(Nm).
  frshn(.tFun(T,A,N),_) => .tFun(T,A,N).
  frshn(.tpFun(N,A),_) => .tpFun(N,A).
  frshn(.tpExp(O,A),Env) => .tpExp(frshnD(O,Env),frshnD(A,Env)).
  frshn(.tupleType(Els),Env) => .tupleType(frshnList(Els,Env)).
  frshn(.faceType(Els,Tps),Env) =>
    .faceType(Els//(((Nm,E))=>(Nm,frshnD(E,Env))),
      Tps//(((Nm,Rl))=>(Nm,freshenRl(Rl,Env)))).
  frshn(.allType(K,T),Env) => valof{
    (Inn,Q,Ev) = skolemQuants(.allType(K,T),[],Env);
    FrTp = frshn(T,declareTypeVars(Q,Ev));
    valis foldLeft(((_,VT),Tp)=>.allType(VT,Tp),FrTp,Q)
  }
  frshn(.existType(K,T),Env) => valof{
    (Inn,Q,Ev) = skolemQuants(.allType(K,T),[],Env);
    FrTp = frshn(T,declareTypeVars(Q,Ev));
    valis foldLeft(((_,VT),Tp)=>.existType(VT,Tp),FrTp,Q)
  }
  frshn(.constrainedType(T,C),Env) => .constrainedType(frshnD(T,Env),frshnConstraint(C,Env)).

  frshnList:(cons[tipe],dict) => cons[tipe].
  frshnList(As,Env) => (As//(E)=>frshn(deRef(E),Env)).

  frshnD(Tp,Env) => frshn(deRef(Tp),Env).

  public refreshConstraint:(cons[(string,tipe)],constraint,dict) => constraint.
  refreshConstraint(Q,T,Env) =>
    frshnConstraint(T,
      foldLeft(((QNm,QTp),E)=>declareType(QNm,.none,QTp,
	  .typeExists(QTp,emptyFace),E),Env,Q)).

  frshnConstraint(.conTract(N,T,D),Env) =>
    .conTract(N,frshnList(T,Env),frshnList(D,Env)).
  frshnConstraint(.hasField(V,F,T),Env) =>
    .hasField(frshnD(V,Env),F,frshnD(T,Env)).
  frshnConstraint(.implicit(N,T),Env) =>
    .implicit(N,frshn(T,Env)).
}
