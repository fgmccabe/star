star.compiler.checker{
  import star.

  import star.pkg.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.dependencies.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.types.
  import star.compiler.typeparse.
  import star.compiler.freshen.
  import star.compiler.unify.
  import star.compiler.wff.

  typeOfExp:(ast,tipe,dict,string,reports) => either[reports,canon].
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) =>
    (Spec ^= isVar(Id,Env) ?
	typeOfVar(Lc,Id,Tp,Spec,Env,Rp) ||
	other(reportError(Rp,"variable $(Id) not defined. Expecting a $(Tp)",Lc))).
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Ix) ^= isInt(A) &&
      (_,IntTp,_) ^= findType(Env,"integer") => do{
	checkType(A,IntTp,Tp,Env,Rp);
	valis intLit(Lc,Ix)
      }
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Dx) ^= isFlt(A) &&
      (_,FltTp,_) ^= findType(Env,"float") => do{
	checkType(A,FltTp,Tp,Env,Rp);
	valis floatLit(Lc,Dx)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Sx) ^= isStr(A) &&
      (_,StrTp,_) ^= findType(Env,"string") => do{
	checkType(A,StrTp,Tp,Env,Rp);
	valis stringLit(Lc,Sx)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,T) ^= isTypeAnnotation(A) => do{
	ETp <- parseType([],T,Env,Rp);
	checkType(E,Tp,ETp,Env,Rp);
	typeOfExp(E,Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,T) ^= isCoerce(A) => do{
	typeOfExp(binary(Lc,":",unary(Lc,"_coerce",E),T),Tp,Env,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,E,C) ^= isWhere(A) => do{
	(Cond,Ev0) <- checkGoal(C,Env,Path,Rp);
	typeOfExp(E,Tp,Ev0,Path,Rp)
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isConjunct(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isDisjunct(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    (Tst,E0) <- checkGoal(T,Env,Path,Rp);
    Thn <- typeOfExp(L,Tp,E0,Path,Rp);
    Els <- typeOfExp(R,Tp,Env,Path,Rp);
    valis cond(Lc,Tst,Thn,Els)
  }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      _ ^= isNegation(A) &&
      (_,BoolTp,_) ^= findType(Env,"boolean") => do{
	checkType(A,BoolTp,Tp,Env,Rp);
	(Gl,_) <- checkGoal(A,Env,Path,Rp);
	valis Gl
      }.
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,R,F) ^= isFieldAcc(A) => do{
	Fce <- recordFace(R,Env,Rp);
	
      }

  checkGoal:(ast,dict,string,reports) => either[reports,(canon,dict)].
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isConjunct(A) => do{
    (Lhs,E0) <- checkGoal(L,Env,Path,Rp);
    (Rhs,E1) <- checkGoal(R,E0,Path,Rp);
    valis (conj(Lc,Lhs,Rhs),E1)
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,L,R) ^= isDisjunct(A) => do{
    (Lhs,E0) <- checkGoal(L,Env,Path,Rp);
    (Rhs,E1) <- checkGoal(R,Env,Path,Rp);
    valis (disj(Lc,Lhs,Rhs),mergeDict(E0,E1,Env))
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,R) ^= isNegation(A) => do{
    (Rhs,_) <- checkGoal(R,Env,Path,Rp);
    valis (neg(Lc,Rhs),Env)
  }.
  checkGoal(A,Env,Path,Rp) where (Lc,T,L,R) ^= isConditional(A) => do{
    (Tst,E0) <- checkGoal(T,Env,Path,Rp);
    (Thn,E1) <- checkGoal(L,E0,Path,Rp);
    (Els,E2) <- checkGoal(R,Env,Path,Rp);
    valis (cond(Lc,Tst,Thn,Els),mergeDict(E1,E2,Env))
  }.
  checkGoal(A,Env,Path,Rp) where (_,BoolTp,_) ^= findType(Env,"boolean") => do{
    Exp <- typeOfExp(A,BoolTp,Env,Path,Rp);
    valis (Exp,Env)
  }
      
  typeOfVar:(locn,string,tipe,vrEntry,dict,reports) => either[reports,canon].
  typeOfVar(Lc,Nm,Tp,vrEntry(_,Mk,VTp),Env,Rp) => do{
    (_,VrTp) = freshen(VTp,[],Env);
    (MTp,Term) <- manageConstraints(VrTp,[],Lc,Mk(Lc,VrTp),Env,Rp);
    if sameType(Tp,MTp,Env) then {
      valis Term
    } else
      throw reportError(Rp,"$(Nm)\:$(VrTp) not consistent with expected type: $(Tp)",Lc)
  }

  manageConstraints:(tipe,list[constraint],locn,canon,dict,reports) => either[reports,(tipe,canon)].
  manageConstraints(constrainedType(Tp,Con),Cons,Lc,Term,Env,Rp)
      where C0 .= applyConstraint(Con,Cons) =>
    manageConstraints(Tp,C0,Lc,Term,Env,Rp).
  manageConstraints(Tp,[],_,Term,Env,_) => either((Tp,Term)).

  applyConstraint(fieldConstraint(T,F),Cons) where
      _ ^= addConstraint(T,fieldConstraint(T,F)) => Cons.
  applyConstraint(typeConstraint(T),Cons) => valof action{
    _ = attachToArgs(T,typeConstraint(T));
    valis [Cons..,typeConstraint(T)]
  }

  attachToArgs(depType(Lhs,_),Con) => attach(Lhs,Con).
  attachToArgs(_,_) => ().
  attach(tpExp(Op,A),Con) where _ ^= addConstraint(A,Con) => attach(Op,Con).
  attach(_,_) => ().
  
  checkType:(ast,tipe,tipe,dict,reports) => either[reports,()].
  checkType(_,Actual,Expected,Env,_) where sameType(Actual,Expected,Env) => either(()).
  checkType(A,ATp,ETp,_,Rp) => other(reportError(
      Rp,"$(A)\:$(ATp) not consistent with expected type $(ETp)",locOf(A))).

  mergeDict:(dict,dict,dict) => dict.
  mergeDict(D1,D2,Env) => let{
    mergeScopes([scope(T1,V1,C1,I1),..Rst],
      [scope(_,V2,_,_),.._]) =>
      [scope(T1,mergeVDefs(V1,V2),C1,I1),..Rst].
    mergeVDefs(V1,V2) => {Nm->E1|Nm->E1 in V1 && Nm->E2 in V2 &&
	  sameDesc(E1,E2)}.
    sameDesc(vrEntry(_,_,T1),vrEntry(_,_,T2)) => sameType(T1,T2,Env)
  } in mergeScopes(D1,D2).
  

}
