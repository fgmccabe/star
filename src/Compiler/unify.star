star.compiler.unify{
  import star.
  import star.sort.

  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.misc.
  import star.compiler.types.

  resetV ::= .resetVar(tipe).

  public sameType:(tipe,tipe,dict) => boolean.
  sameType(Tp1,Tp2,Envir) => let{.
    resets : ref cons[resetV].
    resets = ref [].

    resetBindings() => valof{
      for .resetVar(BndVr) in resets! do{
	resetBinding(BndVr)
      };
      valis .false
    }

    addVarBinding(TV) => valof{
      resets := [.resetVar(TV),..resets!];
      valis ()
    }

    identical(.tVar(_,N),.tVar(_,M)) => N==M.
    identical(.tFun(_,N1,A1),.tFun(_,N2,A2)) => N1==N2 && A1==A2.
    identical(_,_) default => .false.
  .} in let{.
    same(T1,T2,Env) => sm(deRef(T1),deRef(T2),Env).
    
    sm(.kFun(Nm,Ar),.kFun(Nm,Ar),_) => .true.
    sm(T1,T2,Env) where .tVar(_,_) .= T1 => varBinding(T1,T2,Env).
    sm(T1,T2,Env) where .tVar(_,_) .= T2 => varBinding(T2,T1,Env).
    sm(T1,T2,Env) where .tFun(_,_,_) .= T1 => varBinding(T1,T2,Env).
    sm(T1,T2,Env) where .tFun(_,_,_) .= T2 => varBinding(T2,T1,Env).
    sm(T1,T2,Env) default => smT(T1,T2,Env).

    smT(.anonType,_,_) => .true.
    smT(_,.anonType,_) => .true.
    smT(.kVar(Nm),.kVar(Nm),_) => .true.
    smT(.nomnal(Nm),.nomnal(Nm),_) => .true.
    smT(.tpFun(Nm,Ar),.tpFun(Nm,Ar),_) => .true.
    smT(.tpExp(O1,A1),.tpExp(O2,A2),Env) =>
      same(O1,O2,Env) && same(A1,A2,Env).
    smT(.tupleType(A1),.tupleType(A2),Env) =>
      size(A1)==size(A2) && smTypes(A1,A2,Env).
    smT(.faceType(E1,T1),.faceType(E2,T2),Env)
	where size(E1)==size(E2) && size(T1)==size(T2) =>
      smFields(sort(E1,cmpField),sort(E2,cmpField),Env) &&
	  smRules(sort(T1,cmpField),sort(T2,cmpField),Env).
    smT(.existType(V,T1),.existType(V,T2),Env) =>
      same(T1,T2,Env).
    smT(.existType(V1,T1),.existType(V2,T2),Env) =>
      same(T1,rewriteType(T2,[V2->V1]),Env).
    smT(.allType(V,T1),.allType(V,T2),Env) =>
      same(T1,T2,Env).
    smT(.allType(V1,T1),.allType(V2,T2),Env) =>
      same(T1,rewriteType(T2,[V2->V1]),Env).
    smT(.constrainedType(T1,C1),.constrainedType(T2,C2),Env) =>
      same(T1,T2,Env) && sameConstraint(C1,C2,Env).
    smT(T1,T2,_) default => resetBindings().

    smTypes([],[],_) => .true.
    smTypes([E1,..L1],[E2,..L2],Env) =>
      same(E1,E2,Env) && smTypes(L1,L2,Env).
    smTypes(_,_,_) default => resetBindings().

    smFields([],[],_) => .true.
    smFields([(F1,T1),..FS1],[(F2,T2),..FS2],Env) =>
      F1==F2 && same(T1,T2,Env) && smFields(FS1,FS2,Env).

    cmpField:all x ~~ ((string,x),(string,x))=>boolean.
    cmpField((F1,_),(F2,_)) => F1<F2.

    smRules(L1,L2,Q) => let{.
      smRls([],[]) => .true.
      smRls([(N,R1),..Rs1],[(N,R2),..Rs2]) =>
	sameTypeRule(R1,R2,Q) && smRls(Rs1,Rs2).
      smRls(_,_) default => .false.
    .} in smRls(L1,L2).

    sameTypeRule(.typeExists(L1,R1),.typeExists(L2,R2),Env) =>
	same(L1,L2,Env) && same(R1,R2,Env).
    sameTypeRule(.typeLambda(L1,R1),.typeLambda(L2,R2),Env) =>
	same(L1,L2,Env) && same(R1,R2,Env).
    sameTypeRule(.contractExists(N,L1,R1,C1),.contractExists(N,L2,R2,C2),Env) =>
      smTypes(L1,L2,Env) && smTypes(R1,R2,Env) && same(C1,C2,Env).
    sameTypeRule(.allRule(V,B1),.allRule(V,B2),Env) =>
	sameTypeRule(B1,B2,Env).
    sameTypeRule(.allRule(V1,B1),.allRule(V2,B2),Env) =>
	sameTypeRule(B1,rewriteTypeRule(B2,[V2->V1]),Env).
    sameTypeRule(_,_,_) default => .false.

    sameConstraint(.conTract(N1,T1,D1),.conTract(N2,T2,D2),Env) =>
      N1==N2 && smTypes(T1,T2,Env) && smTypes(D1,D2,Env).
    sameConstraint(.hasField(V1,F1,T1),.hasField(V2,F2,T2),Env) =>
      same(V1,V2,Env) && F1==F2 && same(T1,T2,Env).
    sameConstraint(.implicit(N1,T1),.implicit(N2,T2),Env) =>
      N1==N2 && same(T1,T2,Env).
    sameConstraint(.raisEs(T1),.raisEs(T2),Env) => same(T1,T2,Env).
    sameConstraint(_,_,_) default => .false.

    varBinding(T1,T2,_) where isIdenticalVar(T1,T2) => .true.
    varBinding(T1,T2,Env) where ~ occursIn(T1,T2) => 
      bind(T1,T2,Env).
    varBinding(_,_,_) default => resetBindings().

    bind(V,T,Env) where ~identical(V,T) => valof{
      if ~identical(V,T) then{
	setBinding(V,T);
	addVarBinding(V)
      };
      valis .true
    }.
  .} in (sm(deRef(Tp1),deRef(Tp2),Envir) ?? .true || resetBindings()).

  public faceOfType:(tipe,dict) => option[tipe].
  faceOfType(T,_) where .faceType(_,_).=deRef(T) => .some(T).
  faceOfType(T,Env) => valof{
    if (_,_,Rl,_) ?= findType(Env,localName(tpName(T),.typeMark)) then{
      (_,FRl) = freshen(Rl,Env);
      if .typeExists(Lhs,Rhs) .= FRl && sameType(Lhs,T,Env) then{
	(_,RRhs) = freshen(deRef(Rhs),Env);
	valis fcTp(RRhs)
      }
      else
      valis .none
    } else{
      valis .none
    }
  }.
  
  fcTp(.faceType(Flds,Tps)) => .some(.faceType(Flds,Tps)).
  fcTp(_) default => .none.

  rewriteType:(tipe,map[tipe,tipe])=>tipe.
  rewriteType(Tp,Env) => rewr(deRef(Tp),Env).
  
  rewr(.anonType,_) => .anonType.
  rewr(.kFun(Nm,Ar),Env) where T?=Env[.kFun(Nm,Ar)] => T.
  rewr(.kVar(Nm),Env) where T?=Env[.kVar(Nm)] => T.
  rewr(V,_) where isUnbound(V) => V.
  rewr(.kFun(Nm,Ar),Env) => .kFun(Nm,Ar).
  rewr(.nomnal(Nm),Env) => .nomnal(Nm).
  rewr(.tpFun(Nm,Ar),_) => .tpFun(Nm,Ar).
  rewr(.tpExp(Op,A),Env) => .tpExp(rewriteType(Op,Env),rewriteType(A,Env)).
  rewr(.tupleType(Els),Env) => .tupleType(rewriteTps(Els,Env)).
  rewr(.allType(V,B),Env) => _ ?= Env[V] ?? .allType(V,B) || .allType(V,rewriteType(B,Env)).
  rewr(.existType(V,B),Env) => _ ?= Env[V] ?? .existType(V,B) || .existType(V,rewriteType(B,Env)).
  rewr(.faceType(Flds,Rls),Env) =>
    .faceType(Flds//((Nm,T))=>(Nm,rewriteType(T,Env)),
      Rls//((Nm,Rl))=>(Nm,rewriteTypeRule(Rl,Env))).
  rewr(.constrainedType(T,C),Env) => .constrainedType(rewriteType(T,Env),rewriteCon(C,Env)).

  rewriteTps(Tps,Env) => (Tps//(E)=>rewriteType(E,Env)).

  rewriteCon(.conTract(N,T,D),Env) =>
    .conTract(N,rewriteTps(T,Env),rewriteTps(D,Env)).
  rewriteCon(.hasField(V,F,T),Env) =>
    .hasField(rewriteType(V,Env),F,rewriteType(T,Env)).
  rewriteCon(.implicit(N,T),Env) => .implicit(N,rewriteType(T,Env)).
  rewriteCon(.raisEs(T),Env) => .raisEs(rewriteType(T,Env)).

  rewriteTypeRule(.typeExists(L,R),Env) =>
    .typeExists(rewriteType(L,Env),rewriteType(R,Env)).
  rewriteTypeRule(.typeLambda(L,R),Env) =>
    .typeLambda(rewriteType(L,Env),rewriteType(R,Env)).
  rewriteTypeRule(.contractExists(N,L,R,C),Env) =>
    .contractExists(N,rewriteTps(L,Env),rewriteTps(R,Env),rewriteType(deRef(C),Env)).
  rewriteTypeRule(.allRule(V,B),Env) =>
    _ ?= Env[V] ?? .allRule(V,B) || .allRule(V,rewriteTypeRule(B,Env)).
}
