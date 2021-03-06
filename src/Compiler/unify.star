star.compiler.unify{
  import star.
  import star.sort.

  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.freshen.
  import star.compiler.misc.
  import star.compiler.types.

  reset ::= resetVar(tipe) | resetConstraint(tipe,cons[constraint]).

  public sameType:(tipe,tipe,dict) => boolean.
  sameType(Tp1,Tp2,Envir) => let{.
    resets : ref cons[reset].
    resets := [].

    resetBindings = action{
      for Rx in resets! do{
	if resetVar(BndVr) .= Rx then {
	  resetBinding(BndVr)
	} else if resetConstraint(CxV,Cx) .= Rx then {
	  setConstraints(CxV,Cx)
	}
      };
      valis .false
    }

    addVarBinding(TV) => action{
      resets := [resetVar(TV),..resets!];
      valis ()
    }

    addVarConstraints(V) => action{
      resets := [resetConstraint(V,constraintsOf(V)),..resets!];
      valis ()
    }

  .} in let{
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

    sameConstraint(contractConstraint(T1),contractConstraint(T2),Env) => 
      same(T1,T2,Env).
    sameConstraint(fieldConstraint(F1,T1),fieldConstraint(F2,T2),Env) =>
      same(F1,F2,Env) && same(T1,T2,Env).

    updateEnv(nomnal(K),T,Env) => declareType(K,.none,T,faceType([],[]),Env).
    updateEnv(kFun(K,_),T,Env) => declareType(K,.none,T,faceType([],[]),Env).

    varBinding(T1,T2,_) where isIdenticalVar(T1,T2) => .true.
    varBinding(T1,T2,Env) where ~ occursIn(T1,T2) => 
      bind(T1,T2,Env).
    varBinding(_,_,_) default => valof resetBindings.

    bind(V,T,Env) where isUnbound(T) => valof do{
      CV .= constraintsOf(V);
      CT .= constraintsOf(T);
      some(MM) .= mergeConstraints(CV,CT,Env);
      setConstraints(T,MM);
      setBinding(V,T);
      addVarBinding(V);
      valis .true
    }.
    bind(V,T,Env) => valof do {
      VC .= constraintsOf(V);
      try {
	setBinding(V,T);
	addVarBinding(V);
	checkConstraints(VC,Env)
      } catch {
	valis .false
      }
    }.

    checkConstraints:(cons[constraint],dict) => action[(),boolean].
    checkConstraints([],_) => do{ valis .true }.
    checkConstraints([C,..Rest],Env) => do{
      Lhs <- checkConstraint(C,Env);
      if Lhs then
	checkConstraints(Rest,Env)
      else{
	valis .false
      }
    }

    checkConstraint:(constraint,dict) => action[(),boolean].
    checkConstraint(contractConstraint(Tp),Env) => do {
      INm.=implementationName(Tp);
      if Im ^= findImplementation(Env,INm) then{
        (_,FrTp) .= freshen(typeOf(Im),Env);
	(_,DeConTp) .= deConstrain(FrTp);
	valis same(Tp,DeConTp,Env)
      } else{
	valis .true
      }
    }
    checkConstraint(fieldConstraint(T,F),Env) => do{
      Face .= faceOfType(T,Env);
      valis subFace(deRef(F),deRef(Face),Env)
    }.

    mergeConstraints:(cons[constraint],cons[constraint],dict) =>
      option[cons[constraint]].
    mergeConstraints(Cl,Cr,Env) => mergeCons(Cl,Cr,Cr,Env).

    mergeCons:(cons[constraint],cons[constraint],cons[constraint],dict) =>
      option[cons[constraint]].
    mergeCons([],_,Cx,_) => some(Cx).
    mergeCons([C,..Cx],Cy,Sx,Env) where S1^=mergeConstraint(C,Cy,Sx,Env) =>
      mergeCons(Cx,Cy,S1,Env).
    mergeCons(_,_,_,_) default => .none.

    mergeConstraint:(constraint,cons[constraint],cons[constraint],dict) =>
      option[cons[constraint]].
    mergeConstraint(C,[],Cs,_) => some([C,..Cs]).
    mergeConstraint(contractConstraint(Tp),[contractConstraint(Tp1),.._],Cs,Env) =>
      (same(Tp,Tp1,Env) ? some(Cs) || .none).
    -- TODO: handle merging implementsFace more gracefully
    mergeConstraint(C,[_,..Rs],Cs,Env) => mergeConstraint(C,Rs,Cs,Env).
  } in (sm(deRef(Tp1),deRef(Tp2),Envir) ? .true || valof resetBindings).

  subFace(faceType(E1,T1),faceType(E2,T2),Env) => let{.
    subF(Ts1,Ts2) =>
      (Nm,Tp1) in Ts1 *> ((Nm,Tp2) in Ts2 && sameType(Tp1,Tp2,Env)).
  .} in (subF(E1,E2) && subF(T1,T2)).

  public faceOfType:(tipe,dict) => tipe.
  faceOfType(T,_) where faceType(_,_).=deRef(T) => T.
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
      valis faceType([],[])
    } else{
      valis faceType([],[])
    }
  }.
  
  fcTp(faceType(Flds,Tps)) => faceType(Flds,Tps).
  fcTp(_) default => faceType([],[]).

  occursIn(TV,Tp) where ~isIdenticalVar(TV,Tp) =>
      occIn(vrNm(TV),deRef(Tp)).

  occIn(Id,tVar(_,Nm)) => Id==Nm.
  occIn(Id,tFun(_,_,Nm)) => Id==Nm.
  occIn(Id,tpExp(O,A)) => occIn(Id,deRef(O)) || occIn(Id,deRef(A)).
  occIn(Id,tupleType(Els)) => El in Els && occIn(Id,deRef(El)).
  occIn(Id,allType(_,B)) => occIn(Id,deRef(B)).
  occIn(Id,existType(_,B)) => occIn(Id,deRef(B)).
  occIn(Id,faceType(Flds,Tps)) => occInPrs(Id,Flds) || occInPrs(Id,Tps).
  occIn(Id,typeLambda(A,Ra)) => occIn(Id,deRef(A)) || occIn(Id,deRef(Ra)).
  occIn(Id,typeExists(A,Rb)) => occIn(Id,deRef(A)) || occIn(Id,deRef(Rb)).
  occIn(Id,constrainedType(T,_)) => occIn(Id,deRef(T)).
  occIn(_,_) default => .false.

  vrNm(tVar(_,Nm)) => Nm.
  vrNm(tFun(_,_,Nm)) => Nm.

  occInPrs(Id,Tps) => ((_,El) in Tps && occIn(Id,deRef(El))).

  rewriteType:(tipe,map[tipe,tipe])=>tipe.
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

  rewriteCon(contractConstraint(T),Env) => contractConstraint(rewriteType(T,Env)).
  rewriteCon(fieldConstraint(F,T),Env) => fieldConstraint(rewriteType(F,Env),rewriteType(T,Env)).
  
}
