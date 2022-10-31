star.compiler.freshen{
  import star.
  import star.iterable.

  import star.compiler.dict.
  import star.compiler.misc.
  import star.compiler.types.

  public contract all t ~~ fresh[t] ::= {
    freshen:(t,dict) => (cons[(string,tipe)],t).
    refresh:(cons[(string,tipe)],t,dict) => t.
  }

  public implementation fresh[tipe] => {
    freshen(Tp,Env) where (T,Q,Ev) .= freshQuants(deRef(Tp),[],Env) =>
      (Q,frshn(deRef(T),Ev)).
    freshen(Tp,_) default => ([],Tp).

    refresh(Q,T,Env) => frshn(deRef(T),
      foldLeft(((QNm,QTp),E)=>declareType(QNm,.none,QTp,.typeExists(QTp,.faceType([],[])),E),Env,Q)).
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
      freshRule(Rl,
	foldLeft(((Nm,Tp),E)=>declareType(Nm,.none,Tp,.typeExists(Tp,.faceType([],[])),E),
	  Env,Q)).
  }
  
  genQuants:(typeRule,cons[(string,tipe)],dict)=>(cons[(string,tipe)],typeRule,dict).
  genQuants(.allRule(.nomnal(V),R),Q,E) => valof{
    NV = newTypeVar(V);
    valis genQuants(R,[(V,NV),..Q],declareType(V,.none,NV,.typeExists(NV,.faceType([],[])),E))
  }
  genQuants(.allRule(.kFun(V,Ar),R),Q,E) => valof{
    NV = newTypeFun(V,Ar);
    valis genQuants(R,[(V,NV),..Q],declareType(V,.none,NV,.typeExists(NV,.faceType([],[])),E))
  }
  genQuants(R,Q,E) => (Q,R,E).

  freshQ:(tipe,dict) => (tipe,dict).
  freshQ(.nomnal(V),Env) where NV.=newTypeVar(V) =>
    (NV,declareType(V,.none,NV,.typeExists(NV,.faceType([],[])),Env)).
  freshQ(.kFun(V,Ar),Env) where NV.=newTypeFun(V,Ar) =>
    (NV,declareType(V,.none,NV,.typeExists(NV,.faceType([],[])),Env)).

  skolQ:(tipe,dict) => (tipe,dict).
  skolQ(.nomnal(V),Env) where NV.=skolemFun(V,0) =>
    (NV,declareType(V,.none,NV,.typeExists(NV,.faceType([],[])),Env)).
  skolQ(.kFun(V,Ar),Env) where NV.=skolemFun(V,Ar) =>
    (NV,declareType(V,.none,NV,.typeExists(NV,.faceType([],[])),Env)).
  skolQ(Tp,Env) => (Tp,Env).

  freshQuants:(tipe,cons[(string,tipe)],dict)=>(tipe,cons[(string,tipe)],dict).
  freshQuants(.allType(.nomnal(V),T),B,Env) where NV.=newTypeVar(V) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,.faceType([],[])),Env)).
  freshQuants(.allType(.kFun(V,Ar),T),B,Env) where NV.=newTypeFun(V,Ar) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,.faceType([],[])),Env)).
  freshQuants(.existType(.nomnal(V),T),B,Env) where NV.=genSkolemFun(V,B) =>
    freshQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,.faceType([],[])),Env)).
  freshQuants(.existType(V,T),B,Env) =>
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

  skolemQuants(.allType(.nomnal(V),T),B,Env) where .none.=findType(Env,V) =>
    skolemQuants(deRef(T),[(V,.nomnal(V)),..B],Env).
  skolemQuants(.allType(.nomnal(V),T),B,Env) where NV.=skolemFun(V,0) =>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,.faceType([],[])),Env)).
  skolemQuants(.allType(.kFun(V,Ar),T),B,Env)  where .none.=findType(Env,V) =>
    skolemQuants(deRef(T),[(V,.kFun(V,Ar)),..B],declareType(V,.none,.kFun(V,Ar),
	.typeExists(.kFun(V,Ar),.faceType([],[])),Env)).
  skolemQuants(.allType(.kFun(V,Ar),T),B,Env)  where NV.=skolemFun(V,Ar)=>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,.faceType([],[])),Env)).
  skolemQuants(.existType(.nomnal(V),T),B,Env) where NV.=genTypeFun(V,B) =>
    skolemQuants(deRef(T),[(V,NV),..B],declareType(V,.none,NV,
	.typeExists(NV,.faceType([],[])),Env)).
  skolemQuants(T,B,Env) default => (T,B,Env).

  genTypeFun(Nm,[]) => newTypeVar(Nm).
  genTypeFun(Nm,Q) => foldLeft(((_,V),S)=>tpExp(S,V),newTypeFun(Nm,size(Q)),Q).

  frshn:(tipe,dict)=>tipe.
  frshn(.voidType,_) => .voidType.
  frshn(.nomnal(Nm),Env) where (_,Tp,_,_)?=findType(Env,Nm) => Tp.
  frshn(.nomnal(Nm),_) => .nomnal(Nm).
  frshn(.kFun(Nm,Ar),Env) where  (_,Tp,_,_)?=findType(Env,Nm) => Tp.
  frshn(.kFun(Nm,Ar),_) => .kFun(Nm,Ar).
  frshn(.tVar(T,N),_) => .tVar(T,N).
  frshn(.tFun(T,A,N),_) => .tFun(T,A,N).
  frshn(.tpFun(N,A),_) => .tpFun(N,A).
  frshn(.tpExp(O,A),Env) => .tpExp(frshnD(O,Env),frshnD(A,Env)).
  frshn(.throwsType(O,A),Env) => .throwsType(frshnD(O,Env),frshnD(A,Env)).
  frshn(.tupleType(Els),Env) => .tupleType(frshnList(Els,Env)).
  frshn(.faceType(Els,Tps),Env) =>
    .faceType(Els//(((Nm,E))=>(Nm,frshnD(E,Env))),
      Tps//(((Nm,E))=>(Nm,frshnD(E,Env)))).
  frshn(.allType(K,T),Env) => .allType(K,frshn(T,Env)).
  frshn(.existType(K,T),Env) => .existType(K,frshn(T,Env)).
  frshn(.constrainedType(T,C),Env) => .constrainedType(frshnD(T,Env),frshnConstraint(C,Env)).

  frshnList:(cons[tipe],dict) => cons[tipe].
  frshnList(As,Env) => (As//(E)=>frshn(deRef(E),Env)).

  frshnD(Tp,Env) => frshn(deRef(Tp),Env).

  public refreshConstraint:(cons[(string,tipe)],constraint,dict) => constraint.
  refreshConstraint(Q,T,Env) =>
    frshnConstraint(T,
      foldLeft(((QNm,QTp),E)=>declareType(QNm,.none,QTp,
	  .typeExists(QTp,.faceType([],[])),E),Env,Q)).

  frshnConstraint(.conTract(N,T,D),Env) =>
    .conTract(N,frshnList(T,Env),frshnList(D,Env)).
  frshnConstraint(.fieldConstraint(V,F,T),Env) =>
    .fieldConstraint(frshnD(V,Env),F,frshnD(T,Env)).
}
