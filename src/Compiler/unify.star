star.compiler.unify{
  import star.
  import star.iterable.

  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.types.

  public sameType:(tipe,tipe,dict) => boolean.
  sameType(T1,T2,Env) => sm(deRf(T1),deRf(T2),Env).

  sm(voidType,voidType,_) => true.
  sm(thisType,thisType,_) => true.
  sm(kVar(Nm),T2,Env) where (_,T1,_)^=findType(Env,Nm) => sm(deRef(T1),T2,Env).
  sm(T1,kVar(Nm),Env) where (_,T2,_)^=findType(Env,Nm) => sm(T1,deRef(T2),Env).
  sm(kVar(Nm),kVar(Nm),_) => true.
  sm(kFun(Nm,Ar),T2,Env) where (_,T1,_)^=findType(Env,Nm) => sm(deRef(T1),T2,Env).
  sm(T1,kFun(Nm,Ar),Env) where (_,T2,_)^=findType(Env,Nm) => sm(T1,deRef(T2),Env).
  sm(kFun(Nm,Ar),kFun(Nm,Ar),_) => true.
  sm(T1,T2,Env) where tVar(_,_) .= T1 => varBinding(T1,T2,Env).
  sm(T1,T2,Env) where tVar(_,_) .= T2 => varBinding(T2,T1,Env).
  sm(T1,T2,Env) where tFun(_,_,_) .= T1 => varBinding(T1,T2,Env).
  sm(T1,T2,Env) where tFun(_,_,_) .= T2 => varBinding(T2,T1,Env).
  sm(tipe(Nm),tipe(Nm),_) => true.
  sm(tpFun(Nm,Ar),tpFun(Nm,Ar),_) => true.
  sm(tpExp(O1,A1),tpExp(O2,A2),Env) => sameType(O1,O2,Env) && sameType(A1,A2,Env).
  sm(refExp(O1),refExp(O2),Env) => sameType(O1,O2,Env).
  sm(tupleType(A1),tupleType(A2),Env) => size(A1)==size(A2) && smTypes(A1,A2,Env).
  sm(typeLambda(O1,A1),typeLambda(O2,A2),Env) => sameType(O1,O2,Env) && sameType(A1,A2,Env).
  sm(consType(O1,A1),consType(O2,A2),Env) => sameType(O1,O2,Env) && sameType(A1,A2,Env).
  sm(faceType(E1,T1),faceType(E2,T2),Env) where size(E1)==size(E2) && size(T1)==size(T2) => smFields(T1,T2,Env) && smFields(E1,E2,Env).
  sm(existType(V1,T1),existType(V2,T2),Env) => sameType(T1,T2,updateEnv(V1,V2,Env)).
  sm(allType(V1,T1),allType(V2,T2),Env) => sameType(T1,T2,updateEnv(V1,V2,Env)).

  smTypes([],[],_) => true.
  smTypes([E1,..L1],[E2,..L2],Env) => sameType(E1,E2,Env) && smTypes(L1,L2,Env).

  smFields([],[],_) => true.
  smFields([(F1,T1),..FS1],[(F2,T2),..FS2],Env) => F1==F2 && sameType(T1,T2,Env) && smFields(FS1,FS2,Env).

  updateEnv(kVar(K),T,Env) => declareType(K,none,T,Env).
  updateEnv(kFun(K,_),T,Env) => declareType(K,none,T,Env).

  deRf:(tipe,dict)=>tipe.
  deRf(Ky,Env) where Nm^=tpName(deRef(Ky)) && (_,Tp,voidType)^=findType(Env,Nm) => deRef(Tp).
  deRf(Tp,Env) => deRef(Tp).

  tpName:(tipe)=>option[string].
  tpName(kVar(Nm)) => some(Nm).
  tpName(kFun(Nm,_)) => some(Nm).
  tpName(tVar(_,Nm)) => some(Nm).
  tpName(tFun(_,_,Nm)) => some(Nm).
  tpName(tipe(Nm)) => some(Nm).
  tpName(tpFun(Nm,_)) => some(Nm).
  tpName(tpExp(O,_)) => tpName(O).
  tpName(_) => none.


  varBinding(T1,T2,_) where isIdenticalVar(T1,T2) => true.
  varBinding(T1,T2,Env) where \+ occursIn(T1,T2) => bind(T1,T2,Env).
  varBinding(_,_,_) default => false.

  bind(V,T,Env) where isUnbound(T) => setBinding(V,T) && _ ^= setConstraints(V,^mergeConstraints(V,constraintsOf(T),Env)).
  bind(V,T,Env) default => setBinding(V,T) && checkConstraints(constraintsOf(V),Env).

  checkConstraints([],_) => true.
  checkConstraints([C,..Rest],Env) => checkConstraint(C,Env) && checkConstraints(Rest,Env).

  checkConstraint(conConstraint(Nm,Args,Deps),Env) where
      INm.=implementationName(conConstraint(Nm,Args,Deps)) &&
      Im ^= findImplementation(Env,Nm,INm) =>
      sameType(typeOf(conConstraint(Nm,Args,Deps)),typeOf(Im),Env).
  checkConstraint(implConstraint(T,F),Env) where Face ^= faceOfType(T,Env) => subFace(deRef(F),deRef(Face),Env).
  checkConstraint(_,_) default => false.

  subFace(faceType(E1,T1),faceType(E2,T2),Env) => let{.
    subF(Ts1,Ts2) => (Nm,Tp1) in Ts1 *> ((Nm,Tp2) in Ts2 && sameType(Tp1,Tp2,Env)).
  .} in (subF(E1,E2) && subF(T1,T2)).

  public faceOfType:(tipe,dict) => option[tipe].
  faceOfType(T,Env) where Nm^=tpName(T) && (_,_,Rl) ^= findType(Env,Nm) &&
      (_,typeExists(Lhs,Rhs)) .= freshen(Rl,[],Env) &&
      sameType(Lhs,T,Env) => faceOfType(deRef(Rhs),Env).

  faceOfType(faceType(L,T),_) => some(faceType(L,T)).

}
