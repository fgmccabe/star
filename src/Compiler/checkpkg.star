star.compiler.checkpkg{
  import star.
  import star.pkg.
  import star.repo.

  import star.compiler.ast.
  import star.compiler.canon.
  import star.compiler.checker.
  import star.compiler.dependencies.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.impawt.
  import star.compiler.location.
  import star.compiler.meta.
  import star.compiler.misc.
  import star.compiler.types.
  import star.compiler.typeparse.
  import star.compiler.freshen.
  import star.compiler.unify.
  import star.compiler.wff.

  -- package level of type checker

  public checkPkg:all r ~~ repo[r]|:(r,ast,dict,reports) => either[reports,(list[list[canonDef]],list[canon])].
  checkPkg(Repo,P,Base,Rp) => do{
    if (Lc,Pkg,Els) ^= isBrTerm(P) && Path ^= pkgName(Pkg) then{
      (Imports,Stmts) <- collectImports(Els,[],[],Rp);
      (PkgEnv,ImpSigs,AllImports) <- importAll(Imports,Repo,Base,[],[],Rp);
      checkProgram(Els,packageVar(Path),PkgEnv,Rp)
    } else
    throw reportError(Rp,"invalid package structure",locOf(P))
  }

  importAll:all r ~~ repo[r]|:(list[importSpec],r,dict,list[importSpec],
    list[(string,tipe)],reports) => either[reports,(dict,list[(string,tipe)],list[importSpec])].
  importAll([],_,Env,Imported,Sigs,_) => either((Env,Sigs,Imported)).
  importAll([pkgImp(Lc,Viz,Pkg),..Imports],Repo,Env,Imported,Sigs,Rp) => do{
    PkgVar = packageVar(Pkg);
    if (PkgVar,_) in Sigs then
      importAll(Imports,Repo,Env,Imported,Sigs,Rp)
    else{
      if impawtPkg(_,PkgImps,Sig,Cns,Cons,Impls) ^= importPkg(Pkg,Lc,Repo) then {
	E0 = pushSig(Sig,Lc,(L,I,T)=>dot(L,vr(Lc,packageVar(Pkg),Sig),I,T),Env);
	E1 = foldRight((conDfn(_,CNm,CFNm,CTp),EE)=>declareContract(CNm,conDfn(some(Lc),CNm,CFNm,CTp),EE), E0,Cons);
	E2 = foldRight((implDfn(ILc,ConNm,FullNm,Tp),EE)=>
	    declareImplementation(ConNm,FullNm,Tp,EE),E1,Impls);
	importAll(Imports++PkgImps,Repo,E2,[Imported..,pkgImp(Lc,Viz,Pkg)],[Sigs..,(PkgVar,Sig)],Rp)
      } else{
	throw reportError(Rp,"cannot import $(Pkg)",Lc)
      }
    }
  }
}
