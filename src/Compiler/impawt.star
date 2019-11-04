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
      try{
	pkgSpec(_,PkgImps,Sig,Cons,Impls) = valof importPkg(Pkg,Lc,Repo);
	
	E0 = pushSig(Sig,Lc,(I)=>(L,T)=>dot(L,vr(Lc,PkgVar,Sig),I,T),Env);
	E1 = foldRight((conDef(_,CNm,CFNm,CTp),EE)=>
	    declareContract(Lc,CNm,CTp,EE),E0,Cons);
	E2 = foldRight((implSpec(ILc,ConNm,FullNm,Tp),EE)=>
	    declareVr(FullNm,some(Lc),Tp,(LL,TT)=>dot(Lc,vr(Lc,PkgVar,Tp),FullNm,TT),
	      declareImplementation(FullNm,Tp,EE)),E1,Impls);
	importAll(Imports++PkgImps,Repo,E2,[Imported..,pkgImp(Lc,Viz,Pkg)],[Sigs..,(PkgVar,Sig)],Rp)
      }
      catch {
	throw reportError(Rp,"cannot import $(Pkg)",Lc)
      }
    }
  }

  public importPkg:all r ~~ repo[r] |: (pkg,locn,r) => either[(),pkgSpec].
  importPkg(Pkg,Lc,Repo) where Sig ^= hasSignature(Repo,Pkg) => pickupPkgSpec(Sig,Lc).
  importPkg(_,_,_) default => other(()).

  pickupPkgSpec:(string,locn) => either[(),pkgSpec].
  pickupPkgSpec(Txt,Lc) => do{
    (term(_,[Pk,term(_,Imps),strg(FTps),term(_,ConSigs),
	  term(_,ImplSigs)]),R) <- decodeTerm(Txt::list[integer]);
    Pkg <- pickupPkg(Pk);
    Imports <- pickupImports(Imps,Lc);
    Fce <- decodeSignature(FTps);
    Cons <- pickupContracts(ConSigs,Lc,[]);
    Impls <- pickupImplementations(ImplSigs,[]);
    valis pkgSpec(Pkg,Imports,Fce,Cons,Impls)
  }

  pickupPkg:(data) => either[(),pkg].
  pickupPkg(term(lbl("pkg",2),[strg(Nm),V])) => do{
    Vr <- pickupVersion(V);
    valis pkg(Nm,Vr)
  }

  pickupVersion:(data)=>either[(),version].
  pickupVersion(strg("*")) => either(defltVersion).
  pickupVersion(strg(V)) => either(vers(V)).
  pickupVersion(_) default => other(()).

  pickupViz:(data)=>option[visibility].
  pickupViz(strg("private")) => some(priVate).
  pickupViz(strg("public")) => some(pUblic).
  pickupViz(strg("transitive")) => some(transItive).
  pickupVis(_) default => none.

  pickupImports:(list[data],locn) => either[(),list[importSpec]].
  pickupImports(Trms,Lc) => let{
    pickupImps([],Imx) => either(Imx).
    pickupImps([term(_,[V,P]),..Imps],Imx) where
	Vz ^= pickupViz(V) => do{
	  Pkg <- pickupPkg(P);
	  pickupImps(Imps,[Imx..,pkgImp(Lc,Vz,Pkg)])
	}.
  } in pickupImps(Trms,[]).

  pickupConstructors:(list[data],list[string]) => either[(),list[string]].
  pickupConstructors([],Cs) => either(Cs).
  pickupConstructors([strg(Nm),..Ls],Cs) => pickupConstructors(Ls,[Cs..,Nm]).
  pickupConstructors(_,_) default => other(()).

  pickupContracts:(list[data],locn,list[canonDef]) => either[(),list[canonDef]].
  pickupContracts([],_,Cons) => either(Cons).
  pickupContracts([term(_,[strg(Nm),strg(CnNm),strg(Sig)]),..Ts],Lc,Cons) => do{
    Tp <- decodeSignature(Sig);
    pickupContracts(Ts,Lc,[Cons..,conDef(Lc,Nm,CnNm,Tp)])
  }

  pickupImplementations:(list[data],list[implSpec]) => either[(),list[implSpec]].
  pickupImplementations([],Imps) => either(Imps).
  pickupImplementations([term(_,[strg(ConNm),strg(FullNm),strg(Sig)]),..Is],Imps) => do{
    Spec <- decodeSignature(Sig);
    pickupImplementations(Is,[Imps..,implSpec(none,ConNm,FullNm,Spec)])
  }
  pickupImplementations(_,_) default => other(()).

  implementation coercion[pkg,data] => {
    _coerce(pkg(P,defltVersion)) => term(lbl("pkg",2),[strg(P),strg("*")]).
  }

  implementation coercion[visibility,data] => {.
    _coerce(priVate) => strg("private").
    _coerce(pUblic) => strg("public").
    _coerce(transItive) => strg("transitive").
  .}

  implementation coercion[string,data] => {
    _coerce(S) => strg(S)
  }

  implementation coercion[tipe,data] => {.
    _coerce(Tp) => strg(encodeSignature(Tp)).
  .}
  
  implementation coercion[importSpec,data] => {.
    _coerce(pkgImp(_,Vz,Pk)) => term(lbl("import",2),[Vz::data,Pk::data])
  .}

  implementation coercion[canonDef,data] => {.
    _coerce(cnsDef(_,Nm,FullNm,Tp)) =>
      term(lbl("constructor",3),[strg(Nm),strg(FullNm),Tp::data]).
    _coerce(conDef(_,Nm,FullNm,Tp)) =>
      term(lbl("contract",3),[strg(Nm),strg(FullNm),Tp::data]).
  .}
  
  implementation all e ~~ coercion[e,data] |: coercion[list[e],data] => let{
    mkList(L) => term(lbl("()$(size(L))",size(L)),L//(e)=>e::data)
  } in {
    _coerce(L)=>mkList(L)
  }

  implementation coercion[implSpec,data] => {.
    _coerce(implSpec(_,ConNm,FullNm,Spec)) =>
      term(lbl("impl",3),[strg(ConNm),strg(FullNm),Spec::data])
  .}
  
  public implementation coercion[pkgSpec,data] => let{
    mkTerm(pkgSpec(Pkg,Imports,Fields,Contracts,Implementations)) =>
      term(lbl("pkgSpec",5),[Pkg::data,
	  Imports::data,strg(encodeSignature(Fields)),Contracts::data,Implementations::data]).
  } in {
    _coerce(S) => mkTerm(S).
  }
}
