star.compiler.unify{
  import star.
  import star.sort.

  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.misc.
  import star.compiler.types.

  reset ::= resetVar(tipe).

  public sameType:(tipe,tipe,dict) => boolean.
  sameType(Tp1,Tp2,Envir) => let{.
    resets : ref cons[reset].
    resets = ref [].

    resetBindings = action{
      for Rx in resets! do{
	if resetVar(BndVr) .= Rx then {
	  resetBinding(BndVr)
	}
      };
      valis .false
    }

    addVarBinding(TV) => result{
      resets := [resetVar(TV),..resets!];
      valis ()
    }

    identical(tVar(_,N),tVar(_,M)) => N==M.
    identical(tFun(_,N1,A1),tFun(_,N2,A2)) => N1==N2 && A1==A2.
    identical(_,_) default => .false.
  .} in let{.
    same(T1,T2,Env) => sm(deRef(T1),deRef(T2),Env).
    
    sm(kFun(Nm,Ar),kFun(Nm,Ar),_) => .true.
    sm(T1,T2,Env) where tVar(_,_) .= T1 => varBinding(T1,T2,Env).
    sm(T1,T2,Env) where tVar(_,_) .= T2 => varBinding(T2,T1,Env).
    sm(T1,T2,Env) where tFun(_,_,_) .= T1 => varBinding(T1,T2,Env).
    sm(T1,T2,Env) where tFun(_,_,_) .= T2 => varBinding(T2,T1,Env).
    sm(T1,T2,Env) default => smT(T1,T2,Env).

    smT(nomnal(Nm),nomnal(Nm),_) => .true.
    smT(tpFun(Nm,Ar),tpFun(Nm,Ar),_) => .true.
    smT(tpExp(O1,A1),tpExp(O2,A2),Env) =>
      same(O1,O2,Env) && same(A1,A2,Env).
    smT(tupleType(A1),tupleType(A2),Env) =>
      size(A1)==size(A2) && smTypes(A1,A2,Env).
    smT(typeLambda(O1,A1),typeLambda(O2,A2),Env) =>
      same(O1,O2,Env) && same(A1,A2,Env).
    smT(typeExists(O1,A1),typeExists(O2,A2),Env) =>
      same(O1,O2,Env) && same(A1,A2,Env).
    smT(faceType(E1,T1),faceType(E2,T2),Env)
	where size(E1)==size(E2) && size(T1)==size(T2) =>
      smFields(sort(T1,cmpField),sort(T2,cmpField),Env) &&
	  smFields(sort(E1,cmpField),sort(E2,cmpField),Env).
    smT(existType(V,T1),existType(V,T2),Env) =>
      same(T1,T2,Env).
    smT(existType(V1,T1),existType(V2,T2),Env) =>
      same(T1,rewriteType(T2,[V2->V1]),Env).
    smT(allType(V,T1),allType(V,T2),Env) =>
      same(T1,T2,Env).
    smT(allType(V1,T1),allType(V2,T2),Env) =>
      same(T1,rewriteType(T2,[V2->V1]),Env).
    smT(funDeps(T1,D1),funDeps(T2,D2),Env) =>
      same(T1,T2,Env) && smTypes(D1,D2,Env).
    smT(constrainedType(T1,C1),constrainedType(T2,C2),Env) =>
      same(T1,T2,Env) && sameConstraint(C1,C2,Env).
    smT(T1,T2,_) default => valof resetBindings .

    smTypes([],[],_) => .true.
    smTypes([E1,..L1],[E2,..L2],Env) =>
      same(E1,E2,Env) && smTypes(L1,L2,Env).
    smTypes(_,_,_) default => valof resetBindings.

    smFields([],[],_) => .true.
    smFields([(F1,T1),..FS1],[(F2,T2),..FS2],Env) =>
      F1==F2 && same(T1,T2,Env) && smFields(FS1,FS2,Env).

    cmpField:((string,tipe),(string,tipe))=>boolean.
    cmpField((F1,_),(F2,_)) => F1<F2.

    sameConstraint(conTract(T1),conTract(T2),Env) => 
      same(T1,T2,Env).
    sameConstraint(fieldConstraint(V1,F1,T1),fieldConstraint(V2,F2,T2),Env) =>
      same(V1,V2,Env) && F1==F2 && same(T1,T2,Env).

    varBinding(T1,T2,_) where isIdenticalVar(T1,T2) => .true.
    varBinding(T1,T2,Env) where ~ occursIn(T1,T2) => 
      bind(T1,T2,Env).
    varBinding(_,_,_) default => valof resetBindings.

    bind(V,T,Env) where ~identical(V,T) => valof{
      if ~identical(V,T) then{
	setBinding(V,T);
	addVarBinding(V)
      };
      valis .true
    }.
  .} in (sm(deRef(Tp1),deRef(Tp2),Envir) ? .true || valof resetBindings).

  public faceOfType:(tipe,dict) => option[tipe].
  faceOfType(T,_) where faceType(_,_).=deRef(T) => some(T).
  faceOfType(T,Env) => valof action{
    if (_,_,Rl) ^= findType(Env,localName(tpName(T),.typeMark)) then{
      (_,FRl) .= freshen(Rl,Env);
      if typeExists(Lhs,Rhs) .= deRef(FRl) && sameType(Lhs,T,Env) then{
	(_,RRhs) .= freshen(deRef(Rhs),Env);
	valis fcTp(RRhs)
      }
      else if typeLambda(Lhs,Rhs) .= deRef(FRl) && sameType(Lhs,T,Env) then{
	(_,RRhs) .= freshen(deRef(Rhs),Env);
	valis fcTp(RRhs)
      }
      else
      valis .none
    } else{
      valis .none
    }
  }.
  
  fcTp(faceType(Flds,Tps)) => some(faceType(Flds,Tps)).
  fcTp(_) default => .none.

  occursIn(TV,Tp) where ~isIdenticalVar(TV,Tp) =>
      occIn(vrNm(TV),deRef(Tp)).

  occIn(Id,tVar(_,Nm)) => Id==Nm.
  occIn(Id,tFun(_,_,Nm)) => Id==Nm.
  occIn(Id,tpExp(O,A)) => occIn(Id,deRef(O)) || occIn(Id,deRef(A)).
  occIn(Id,tupleType(Els)) => {? El in Els && occIn(Id,deRef(El)) ?}.
  occIn(Id,allType(_,B)) => occIn(Id,deRef(B)).
  occIn(Id,existType(_,B)) => occIn(Id,deRef(B)).
  occIn(Id,faceType(Flds,Tps)) => occInPrs(Id,Flds) || occInPrs(Id,Tps).
  occIn(Id,typeLambda(A,Ra)) => occIn(Id,deRef(A)) || occIn(Id,deRef(Ra)).
  occIn(Id,typeExists(A,Rb)) => occIn(Id,deRef(A)) || occIn(Id,deRef(Rb)).
  occIn(Id,funDeps(T,D)) =>
    occIn(Id,deRef(T)) || {? El in D && occIn(Id,deRef(El)) ?}.
  occIn(Id,constrainedType(T,_)) => occIn(Id,deRef(T)).
  occIn(_,_) default => .false.

  vrNm(tVar(_,Nm)) => Nm.
  vrNm(tFun(_,_,Nm)) => Nm.

  occInPrs(Id,Tps) => {? (_,El) in Tps && occIn(Id,deRef(El)) ?}.

  rewriteType:(tipe,map[tipe,tipe])=>tipe.
  rewriteType(Tp,Env) => rewr(deRef(Tp),Env).
  
  rewr(kFun(Nm,Ar),Env) where T^=Env[kFun(Nm,Ar)] => T.
  rewr(nomnal(Nm),Env) where T^=Env[nomnal(Nm)] => T.
  rewr(V,_) where isUnbound(V) => V.
  rewr(kFun(Nm,Ar),Env) => kFun(Nm,Ar).
  rewr(nomnal(Nm),Env) => nomnal(Nm).
  rewr(tpFun(Nm,Ar),_) => tpFun(Nm,Ar).
  rewr(tpExp(Op,A),Env) => tpExp(rewriteType(Op,Env),rewriteType(A,Env)).
  rewr(tupleType(Els),Env) => tupleType(rewriteTps(Els,Env)).
  rewr(allType(V,B),Env) => _ ^= Env[V] ? allType(V,B) || allType(V,rewriteType(B,Env)).
  rewr(existType(V,B),Env) => _ ^= Env[V] ? existType(V,B) || existType(V,rewriteType(B,Env)).
  rewr(faceType(Flds,Tps),Env) => faceType(Flds//((Nm,T))=>(Nm,rewriteType(T,Env)),
    Tps//((Nm,T))=>(Nm,rewriteType(T,Env))).
  rewr(typeLambda(A,R),_) => typeLambda(A,R). -- fix me
  rewr(typeExists(A,R),_) => typeExists(A,R). -- me too
  rewr(funDeps(T,D),Env) => funDeps(rewriteType(T,Env),rewriteTps(D,Env)).
  rewr(constrainedType(T,C),Env) => constrainedType(rewriteType(T,Env),rewriteCon(C,Env)).

  rewriteTps(Tps,Env) => (Tps//(E)=>rewriteType(E,Env)).

  rewriteCon(conTract(T),Env) =>
    conTract(rewriteType(T,Env)).
  rewriteCon(fieldConstraint(V,F,T),Env) => fieldConstraint(rewriteType(V,Env),F,rewriteType(T,Env)).
}
