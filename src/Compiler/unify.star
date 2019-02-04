star.compiler.unify{
  import star.
  import star.iterable.

  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.types.

  public sameType:(tipe,tipe,dict) => boolean.
  sameType(T1,T2,Env) => sm(deRef(T1),deRef(T2),Env).

  sm(kVar(Nm),kVar(Nm),_) => true.
  sm(kVar(Nm),T2,Env) where (_,T1,_)^=findType(Env,Nm) => sm(deRef(T1),T2,Env).
  sm(T1,kVar(Nm),Env) where (_,T2,_)^=findType(Env,Nm) => sm(T1,deRef(T2),Env).
  sm(kFun(Nm,Ar),T2,Env) where (_,T1,_)^=findType(Env,Nm) => sm(deRef(T1),T2,Env).
  sm(T1,kFun(Nm,Ar),Env) where (_,T2,_)^=findType(Env,Nm) => sm(T1,deRef(T2),Env).
  sm(kFun(Nm,Ar),kFun(Nm,Ar),_) => true.
  sm(T1,T2,Env) where tVar(_,_) .= T1 => varBinding(T1,T2,Env).
  sm(T1,T2,Env) where tVar(_,_) .= T2 => varBinding(T2,T1,Env).
  sm(T1,T2,Env) where tFun(_,_,_) .= T1 => varBinding(T1,T2,Env).
  sm(T1,T2,Env) where tFun(_,_,_) .= T2 => varBinding(T2,T1,Env).
  sm(T1,T2,Env) default => smT(T1,T2,Env).

  smT(voidType,voidType,_) => true.
  smT(thisType,thisType,_) => true.
  smT(tipe(Nm),tipe(Nm),_) => true.
  smT(tpFun(Nm,Ar),tpFun(Nm,Ar),_) => true.
  smT(tpExp(O1,A1),tpExp(O2,A2),Env) => sameType(O1,O2,Env) && sameType(A1,A2,Env).
  smT(refType(O1),refType(O2),Env) => sameType(O1,O2,Env).
  smT(tupleType(A1),tupleType(A2),Env) => size(A1)==size(A2) && smTypes(A1,A2,Env).
  smT(typeLambda(O1,A1),typeLambda(O2,A2),Env) => sameType(O1,O2,Env) && sameType(A1,A2,Env).
  smT(faceType(E1,T1),faceType(E2,T2),Env) where size(E1)==size(E2) && size(T1)==size(T2) => smFields(T1,T2,Env) && smFields(E1,E2,Env).
  smT(existType(V1,T1),existType(V2,T2),Env) => sameType(T1,T2,updateEnv(V1,V2,Env)).
  smT(allType(V1,T1),allType(V2,T2),Env) => sameType(T1,T2,updateEnv(V1,V2,Env)).
  smT(_,_,_) default => false.

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

  bind(V,T,Env) where isUnbound(T) =>
    valof do{
      CV = constraintsOf(V);
      CT = constraintsOf(T);
      MM = valof mergeConstraints(CV,CT,Env);
      setConstraints(T,MM);
      setBinding(V,T);
      return true
    }.
  bind(V,T,Env) => valof do {
    VC = constraintsOf(V);
    {
      setBinding(V,T);
      return checkConstraints(VC,Env)
    } >>> (E) => do {
      resetBinding(V);
      return false
    }
  }

  bind(V,T,Env) where isUnbound(T) =>
        (MM ^= mergeConstraints(constraintsOf(V),constraintsOf(T),Env) ?
          valof do{
            setConstraints(T,MM);
            setBinding(V,T);
            return true
          }
        || false).
  bind(_,_,_) default => false.

  checkConstraints([],_) => true.
  checkConstraints([C,..Rest],Env) => checkConstraint(C,Env) && checkConstraints(Rest,Env).

  checkConstraint(conConstraint(Nm,Args,Deps),Env) where
      INm.=implementationName(conConstraint(Nm,Args,Deps)) &&
      Im ^= findImplementation(Env,Nm,INm) =>
      sameType(typeOf(conConstraint(Nm,Args,Deps)),typeOf(Im),Env).
  checkConstraint(implConstraint(T,F),Env) where Face ^= faceOfType(T,Env) => subFace(deRef(F),deRef(Face),Env).
  checkConstraint(_,_) default => false.

  mergeConstraints:(list[constraint],list[constraint],dict) => option[list[constraint]].
  mergeConstraints(Cl,Cr,Env) => mergeCons(Cl,Cr,Cr,Env).

  mergeCons:(list[constraint],list[constraint],list[constraint],dict) => option[list[constraint]].
  mergeCons([],_,Cx,_) => some(Cx).
  mergeCons([C,..Cx],Cy,Sx,Env) where S1^=mergeConstraint(C,Cy,Sx,Env) =>
    mergeCons(Cx,Cy,S1,Env).
  mergeCons(_,_,_,_) default => none.

  mergeConstraint:(constraint,list[constraint],list[constraint],dict) => option[list[constraint]].
  mergeConstraint(C,[],Cs,_) => some([C,..Cs]).
  mergeConstraint(conConstraint(Nm,A,D),[conConstraint(Nm,A1,D1),.._],Cs,Env) =>
    (sameContract(conConstraint(Nm,A,D),conConstraint(Nm,A1,D1),Env) ? some(Cs) || none).
  -- TODO: handle merging implementsFace more gracefully
  mergeConstraint(C,[_,..R],Cs,Env) => mergeConstraint(C,R,Cs,Env).

  sameContract(conConstraint(Nm,A,D),conConstraint(Nm,A1,D1),Env) =>
    smTypes(A,A1,Env) && smTypes(D,D1,Env).
  sameContract(_,_,_) default => false.

  subFace(faceType(E1,T1),faceType(E2,T2),Env) => let{.
    subF(Ts1,Ts2) => (Nm,Tp1) in Ts1 *> ((Nm,Tp2) in Ts2 && sameType(Tp1,Tp2,Env)).
  .} in (subF(E1,E2) && subF(T1,T2)).

  public faceOfType:(tipe,dict) => option[tipe].
  faceOfType(T,Env) where Nm^=tpName(T) && (_,_,Rl) ^= findType(Env,Nm) &&
      (_,typeExists(Lhs,Rhs)) .= freshen(Rl,[],Env) &&
      sameType(Lhs,T,Env) => faceOfType(deRef(Rhs),Env).

  faceOfType(faceType(L,T),_) => some(faceType(L,T)).

  occursIn(TV,Tp) where \+ isIdenticalVar(TV,Tp) =>
      occIn(vrNm(TV),deRef(Tp)).

  occIn(Id,tVar(_,Nm)) => Id==Nm.
  occIn(Id,tFun(_,_,Nm)) => Id==Nm.
  occIn(Id,tpExp(O,A)) => occIn(Id,deRef(O)) || occIn(Id,deRef(A)).
  occIn(Id,tupleType(Els)) => El in Els && occIn(Id,deRef(El)).
  occIn(Id,refType(B)) => occIn(Id,deRef(B)).
  occIn(Id,allType(_,B)) => occIn(Id,deRef(B)).
  occIn(Id,existType(_,B)) => occIn(Id,deRef(B)).
  occIn(Id,faceType(Flds,Tps)) => occInPrs(Id,Flds) || occInPrs(Id,Tps).
  occIn(Id,typeLambda(A,R)) => occIn(Id,deRef(A)) || occIn(Id,deRef(R)).
  occIn(Id,typeExists(A,R)) => occIn(Id,deRef(A)) || occIn(Id,deRef(R)).
  occIn(Id,constrainedType(T,_)) => occIn(Id,deRef(T)).
  occIn(_,_) default => false.

  vrNm(tVar(_,Nm)) => Nm.
  vrNm(tFun(_,_,Nm)) => Nm.

  occInPrs(Id,Tps) => ((_,El) in Tps && occIn(Id,deRef(El))).
}
