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

  public importAll:all r ~~ repo[r]|:(list[importSpec],r,dict,list[importSpec],
    list[(string,tipe)],reports) => either[reports,(dict,list[importSpec])].
  importAll([],_,Env,Imported,Sigs,_) => either((Env,Imported)).
  importAll([pkgImp(Lc,Viz,Pkg),..Imports],Repo,Env,Imported,Sigs,Rp) => do{
    PkgVar = packageVar(Pkg);
    if (PkgVar,_) in Sigs then
      importAll(Imports,Repo,Env,Imported,Sigs,Rp)
    else{
      if pkgSpec(_,PkgImps,Sig,Cons,Impls) ^= importPkg(Pkg,Lc,Repo) then {
	E0 = pushSig(Sig,Lc,(L,I,T)=>dot(L,vr(Lc,PkgVar,Sig),I,T),Env);
	E1 = foldRight((conDef(_,CNm,CFNm,CTp),EE)=>
	    declareContract(Lc,CNm,CTp,EE),E0,Cons);
	E2 = foldRight((implSpec(ILc,ConNm,FullNm,Tp),EE)=>
	    declareVr(FullNm,some(Lc),Tp,(LL,NN,TT)=>dot(Lc,vr(Lc,PkgVar,Tp),FullNm,TT),
	      declareImplementation(ConNm,FullNm,Tp,EE)),E1,Impls);
	importAll(Imports++PkgImps,Repo,E2,[Imported..,pkgImp(Lc,Viz,Pkg)],[Sigs..,(PkgVar,Sig)],Rp)
      } else{
	throw reportError(Rp,"cannot import $(Pkg)",Lc)
      }
    }
  }

  public importPkg:all r ~~ repo[r] |: (pkg,locn,r) => option[pkgSpec].
  importPkg(Pkg,Lc,Repo) where Sig ^= hasSignature(Repo,Pkg) => pickupPkgSpec(Sig,Lc).
  importPkg(_,_,_) default => none.

  pickupPkgSpec:(string,locn) => option[pkgSpec].
  pickupPkgSpec(Txt,Lc) => do{
    (term(_,[Pk,term(_,Imps),strg(FTps),term(_,ConSigs),
	  term(_,ImplSigs)]),R) <- decodeTerm(Txt::list[integer]);
    Pkg <- pickupPkg(Pk);
    logMsg("imported package $(Pkg)");
    Imports <- pickupImports(Imps,Lc);
    logMsg("transative imports $(Imports)");
    Fce <- decodeSignature(FTps);
    logMsg("imported type sig: $(Fce)");
    Cons <- pickupContracts(ConSigs,Lc,[]);
    logMsg("imported contracts $(Cons)");
    Impls <- pickupImplementations(ImplSigs,[]);
    logMsg("imported implementations $(ImplSigs) -> $(Impls)");
    valis pkgSpec(Pkg,Imports,Fce,Cons,Impls)
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

  pickupContracts:(list[term],locn,list[canonDef]) => option[list[canonDef]].
  pickupContracts([],_,Cons) => some(Cons).
  pickupContracts([term(_,[strg(Nm),strg(CnNm),strg(Sig)]),..Ts],Lc,Cons) => do{
    Tp <- decodeSignature(Sig);
    pickupContracts(Ts,Lc,[Cons..,conDef(Lc,Nm,CnNm,Tp)])
  }

  pickupImplementations:(list[term],list[implSpec]) => option[list[implSpec]].
  pickupImplementations([],Imps) => some(Imps).
  pickupImplementations([term(_,[strg(ConNm),strg(FullNm),strg(Sig)]),..Is],Imps) => do{
    Spec <- decodeSignature(Sig);
    pickupImplementations(Is,[Imps..,implSpec(none,ConNm,FullNm,Spec)])
  }
  pickupImplementations(_,_) default => none.

  implementation coercion[pkg,term] => {
    _coerce(pkg(P,defltVersion)) => term(lbl("pkg",2),[strg(P),enum("*")]).
  }

  implementation coercion[visibility,term] => {.
    _coerce(priVate) => enum("private").
    _coerce(pUblic) => enum("public").
    _coerce(transItive) => enum("transitive").
  .}

  implementation coercion[string,term] => {
    _coerce(S) => strg(S)
  }

  implementation coercion[tipe,term] => {.
    _coerce(Tp) => strg(encodeSignature(Tp)).
  .}
  
  implementation coercion[importSpec,term] => {.
    _coerce(pkgImp(_,Vz,Pk)) => term(lbl("import",2),[Vz::term,Pk::term])
  .}

  implementation coercion[canonDef,term] => {.
    _coerce(cnsDef(_,Nm,FullNm,Tp)) =>
      term(lbl("constructor",3),[strg(Nm),strg(FullNm),Tp::term]).
    _coerce(conDef(_,Nm,FullNm,Tp)) =>
      term(lbl("contract",3),[strg(Nm),strg(FullNm),Tp::term]).
  .}
  
  implementation all e ~~ coercion[e,term] |: coercion[list[e],term] => let{
    mkList(L) => term(lbl("()$(size(L))",size(L)),L//(e)=>e::term)
  } in {
    _coerce(L)=>mkList(L)
  }

  implementation coercion[implSpec,term] => {.
    _coerce(implSpec(_,ConNm,FullNm,Spec)) =>
      term(lbl("impl",3),[strg(ConNm),strg(FullNm),Spec::term])
  .}
  
  public implementation coercion[pkgSpec,term] => let{
    mkTerm(pkgSpec(Pkg,Imports,Fields,Contracts,Implementations)) =>
      term(lbl("pkgSpec",5),[Pkg::term,
	  Imports::term,strg(encodeSignature(Fields)),Contracts::term,Implementations::term]).
  } in {
    _coerce(S) => mkTerm(S).
  }
}
