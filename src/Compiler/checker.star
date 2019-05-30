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
  import star.compiler.freshen.
  import star.compiler.unify.
  import star.compiler.wff.

  typeOfExp:(ast,tipe,dict,string,reports) => either[reports,(canon,dict)].
  typeOfExp(A,Tp,Env,Path,Rp) where (Lc,Id) ^= isName(A) =>
    (Spec ^= isVar(Id,Env) ?
	typeOfVar(Lc,Id,Tp,Spec,Env,Rp) ||
	other(reportError(Rp,"variable $(Id) not defined. Expecting a $(Tp)",Lc))).
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Ix) ^= isInt(A) &&
      (_,IntTp,_) ^= findType(Env,"integer") => do{
	_ <- checkType(A,IntTp,Tp,Env,Rp);
	valis (intLit(Lc,Ix),Env)
      }
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Dx) ^= isFlt(A) &&
      (_,FltTp,_) ^= findType(Env,"float") => do{
	_ <- checkType(A,FltTp,Tp,Env,Rp);
	valis (floatLit(Lc,Dx),Env)
      }
  typeOfExp(A,Tp,Env,Path,Rp) where
      (Lc,Sx) ^= isStr(A) &&
      (_,StrTp,_) ^= findType(Env,"string") => do{
	_ <- checkType(A,StrTp,Tp,Env,Rp);
	valis (stringLit(Lc,Sx),Env)
      }
    

  typeOfVar:(locn,string,tipe,vrEntry,dict,reports) => either[reports,(canon,dict)].
  typeOfVar(Lc,Nm,Tp,vrEntry(_,Mk,VTp),Env,Rp) => do{
    (_,VrTp) = freshen(VTp,[],Env);
    (E0,MTp,Term) <- manageConstraints(VrTp,[],Lc,Mk(Lc,VrTp),Env,Rp);
    if sameType(Tp,MTp,E0) then {
      valis (Term,E0)
    } else
      throw reportError(Rp,"$(Nm)\:$(VrTp) not consistent with expected type: $(Tp)",Lc)
  }

  manageConstraints:(tipe,list[constraint],locn,canon,dict,reports) => either[reports,(dict,tipe,canon)].
  manageConstraints(constrainedType(Tp,Con),Cons,Lc,Term,Env,Rp)
      where C0 .= applyConstraint(Con,Cons) =>
    manageConstraints(Tp,C0,Lc,Term,Env,Rp).
  manageConstraints(Tp,[],_,Term,Env,_) => either((Env,Tp,Term)).

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
}
