star.compiler.impawt{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.meta.
  import star.compiler.terms.
  import star.compiler.types.

  public impawtSpec::=impawtPkg(pkg,list[importSpec],tipe,list[string],list[contractDefn],list[implDefn]).

  public importAll:all r ~~ repo[r]|:(list[importSpec],r,dict,list[importSpec],
    list[(string,tipe)],reports) => either[reports,(dict,list[importSpec])].
  importAll([],_,Env,Imported,Sigs,_) => either((Env,Imported)).
  importAll([pkgImp(Lc,Viz,Pkg),..Imports],Repo,Env,Imported,Sigs,Rp) => do{
    PkgVar = packageVar(Pkg);
    if (PkgVar,_) in Sigs then
      importAll(Imports,Repo,Env,Imported,Sigs,Rp)
    else{
      if impawtPkg(_,PkgImps,Sig,Cns,Cons,Impls) ^= importPkg(Pkg,Lc,Repo) then {
	E0 = pushSig(Sig,Lc,(L,I,T)=>dot(L,vr(Lc,PkgVar,Sig),I,T),Env);
	E1 = foldRight((conDfn(_,CNm,CFNm,CTp),EE)=>
	    declareContract(CNm,conDfn(some(Lc),CNm,CFNm,CTp),EE), E0,Cons);
	E2 = foldRight((implDfn(ILc,ConNm,FullNm,Tp),EE)=>
	    declareImplementation(ConNm,FullNm,Tp,EE),E1,Impls);
	importAll(Imports++PkgImps,Repo,E2,[Imported..,pkgImp(Lc,Viz,Pkg)],[Sigs..,(PkgVar,Sig)],Rp)
      } else{
	throw reportError(Rp,"cannot import $(Pkg)",Lc)
      }
    }
  }

  public importPkg:all r ~~ repo[r] |: (pkg,locn,r) => option[impawtSpec].
  importPkg(Pkg,Lc,Repo) where Sig ^= hasSignature(Repo,Pkg) => pickupPkgSpec(Sig,Lc).
  importPkg(_,_,_) default => none.

  pickupPkgSpec:(string,locn) => option[impawtSpec].
  pickupPkgSpec(Txt,Lc) => do{
    (term(_,[Pk,term(_,Imps),strg(FTps),term(_,ClSigs),term(_,ConSigs),
	  term(_,ImplSigs)]),R) <- decodeTerm(Txt::list[integer]);
    Pkg <- pickupPkg(Pk);
    logMsg("imported package $(Pkg)");
    Imports <- pickupImports(Imps,Lc);
    logMsg("transative imports $(Imports)");
    Fce <- decodeSignature(FTps);
    logMsg("imported type sig: $(Fce)");
    Cns <- pickupConstructors(ClSigs,[]);
    logMsg("imported constructors $(Cns)");
    Cons <- pickupContracts(ConSigs,[]);
    logMsg("imported contracts $(Cons)");
    Impls <- pickupImplementations(ImplSigs,[]);
    logMsg("imported implementations $(Impls)");
    valis impawtPkg(Pkg,Imports,Fce,Cns,Cons,Impls)
  }

  pickupPkg:(term) => option[pkg].
  pickupPkg(term(lbl("pkg",2),[strg(Nm),V])) => do{
    Vr <- pickupVersion(V);
    valis pkg(Nm,Vr)
  }

  pickupVersion:(term)=>option[version].
  pickupVersion(enum("*")) => some(defltVersion).
  pickupVersion(strg(V)) => some(vers(V)).
  pickupVersion(_) default => none.

  pickupViz:(term)=>option[visibility].
  pickupViz(enum("private")) => some(priVate).
  pickupViz(enum("public")) => some(pUblic).
  pickupViz(enum("transitive")) => some(transItive).
  pickupVis(_) default => none.

  pickupImports:(list[term],locn) => option[list[importSpec]].
  pickupImports(Trms,Lc) => let{
    pickupImps([],Imx) => some(Imx).
    pickupImps([term(_,[V,P]),..Imps],Imx) where
	Vz ^= pickupViz(V) &&
	Pkg ^= pickupPkg(P) => pickupImps(Imps,[Imx..,pkgImp(Lc,Vz,Pkg)]).
  } in pickupImps(Trms,[]).

  pickupConstructors:(list[term],list[string]) => option[list[string]].
  pickupConstructors([],Cs) => some(Cs).
  pickupConstructors([strg(Nm),..Ls],Cs) => pickupConstructors(Ls,[Cs..,Nm]).
  pickupConstructors(_,_) default => none.

  pickupContracts:(list[term],list[contractDefn]) => option[list[contractDefn]].
  pickupContracts([],Cons) => some(Cons).
  pickupContracts([term(_,[strg(Nm),strg(CnNm),strg(Sig)]),..Ts],Cons) => do{
    Tp <- decodeSignature(Sig);
    pickupContracts(Ts,[Cons..,conDfn(none,Nm,CnNm,Tp)])
  }

  pickupImplementations:(list[term],list[implDefn]) => option[list[implDefn]].
  pickupImplementations([],Imps) => some(Imps).
  pickupImplementations([term(_,[strg(ConNm),strg(FullNm),strg(Sig)]),..Is],Imps) => do{
    Spec <- decodeSignature(Sig);
    pickupImplementations(Is,[Imps..,implDfn(none,ConNm,FullNm,Spec)])
  }
  pickupImplementations(_,_) default => none.
  
}
