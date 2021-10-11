star.compiler.impawt{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.canon.
  import star.compiler.dict.
  import star.compiler.dict.mgt.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.meta.
  import star.compiler.terms.
  import star.compiler.types.

  public importAll:all r ~~ repo[r],display[r]|:(cons[importSpec],r,dict,cons[importSpec],
    cons[(string,tipe)],reports) => either[reports,(dict,cons[importSpec],cons[(string,tipe)])].
  importAll([],_,Env,Imported,Sigs,_) => either((Env,Imported,Sigs)).
  importAll([pkgImp(Lc,Viz,Pkg),..Imports],Repo,Env,Imported,Sigs,Rp) => do{
    PkgVar .= packageVar(Pkg);
    if (PkgVar,_) in Sigs then
      importAll(Imports,Repo,Env,Imported,Sigs,Rp)
    else{
      try{
	pkgSpec(_,PkgImps,Sig,Cons,Impls,_) <- importPkg(Pkg,Lc,Repo,Rp);
	
	E0 .= pushSig(Sig,Lc,
	  (Id,T,E) where (_,DQ).=deQuant(T)=> (_ ^= isConsType(DQ) ?
	      declareConstructor(Id,qualifiedName(pkgName(Pkg),.conMark,Id),some(Lc),T,E) ||
	      declareFldAccess(vr(Lc,PkgVar,Sig),Id,some(Lc),T,E)),Env);
	E1 .= foldRight((conDef(_,CNm,CFNm,CTp),EE)=>
	    declareContract(Lc,CNm,CTp,EE),E0,Cons);
	E2 .= foldRight((implSpec(ILc,ConNm,FullNm,Tp),EE)=>
	    declareFldAccess(vr(Lc,PkgVar,Sig),FullNm,some(Lc),Tp,
	      declareImplementation(FullNm,Tp,EE)),E1,Impls);
	importAll(Imports++PkgImps,Repo,E2,[pkgImp(Lc,Viz,Pkg),..Imported],
	  [(PkgVar,Sig),..Sigs],Rp)
      }
      catch {
	raise reportError(Rp,"cannot import $(Pkg)",Lc)
      }
    }
  }

  public importPkg:all r ~~ repo[r] |: (pkg,locn,r,reports) => either[reports,pkgSpec].
  importPkg(Pkg,Lc,Repo,Rp) where Sig ^= pkgSignature(Repo,Pkg) => either (valof pickupPkgSpec(Sig,Lc,Rp)).
  importPkg(Pkg,Lc,_,Rp) default => other(reportError(Rp,"cannot import $(Pkg)",Lc)).

  pickupPkgSpec:(string,locn,reports) => either[(),pkgSpec].
  pickupPkgSpec(Txt,Lc,Rp) => do{
    (term(_,[Pk,term(_,Imps),strg(FTps),term(_,ConSigs),
	  term(_,ImplSigs)]),R) <- decodeTerm(Txt::cons[integer]);
    Pkg <- pickupPkg(Pk);
    Imports <- pickupImports(Imps,Lc);
    Fce <- decodeSignature(FTps);
    Cons <- pickupContracts(ConSigs,Lc,[]);
    Impls <- pickupImplementations(ImplSigs,[]);
    valis pkgSpec(Pkg,Imports,Fce,Cons,Impls,[(pkgName(Pkg),Fce)])
  }

  pickupPkg:(term) => either[(),pkg].
  pickupPkg(term(tLbl("pkg",2),[strg(Nm),V])) => do{
    Vr <- pickupVersion(V);
    valis pkg(Nm,Vr)
  }

  pickupVersion:(term)=>either[(),version].
  pickupVersion(strg("*")) => either(.defltVersion).
  pickupVersion(strg(V)) => either(vers(V)).
  pickupVersion(_) default => other(()).

  pickupViz:(term)=>option[visibility].
  pickupViz(strg("private")) => some(.priVate).
  pickupViz(strg("public")) => some(.pUblic).
  pickupViz(strg("transitive")) => some(.transItive).
  pickupVis(_) default => .none.

  pickupImports:(cons[term],locn) => either[(),cons[importSpec]].
  pickupImports(Trms,Lc) => let{
    pickupImps([],Imx) => either(Imx).
    pickupImps([term(_,[V,P]),..Imps],Imx) where
	Vz ^= pickupViz(V) => do{
	  Pkg <- pickupPkg(P);
	  pickupImps(Imps,[pkgImp(Lc,Vz,Pkg),..Imx])
	}.
  } in pickupImps(Trms,[]).

  pickupConstructors:(cons[term],cons[string]) => either[(),cons[string]].
  pickupConstructors([],Cs) => either(Cs).
  pickupConstructors([strg(Nm),..Ls],Cs) => pickupConstructors(Ls,[Nm,..Cs]).
  pickupConstructors(_,_) default => other(()).

  pickupContracts:(cons[term],locn,cons[canonDef]) => either[(),cons[canonDef]].
  pickupContracts([],_,Cons) => either(Cons).
  pickupContracts([term(_,[strg(Nm),strg(CnNm),strg(Sig)]),..Ts],Lc,Cons) => do{
    Tp <- decodeSignature(Sig);
    pickupContracts(Ts,Lc,[conDef(Lc,Nm,CnNm,Tp),..Cons])
  }

  pickupImplementations:(cons[term],cons[implSpec]) => either[(),cons[implSpec]].
  pickupImplementations([],Imps) => either(Imps).
  pickupImplementations([term(_,[strg(ConNm),strg(FullNm),strg(Sig)]),..Is],Imps) => do{
    Spec <- decodeSignature(Sig);
    pickupImplementations(Is,[implSpec(.none,ConNm,FullNm,Spec),..Imps])
  }
  pickupImplementations(_,_) default => other(()).

  implementation coercion[pkg,term] => {
    _coerce(pkg(P,.defltVersion)) => some(term(tLbl("pkg",2),[strg(P),strg("*")])).
    _coerce(pkg(P,vers(V))) => some(term(tLbl("pkg",2),[strg(P),strg(V)])).
  }

  implementation coercion[visibility,term] => {.
    _coerce(.priVate) => some(strg("private")).
    _coerce(.pUblic) => some(strg("public")).
    _coerce(.transItive) => some(strg("transitive")).
  .}

  implementation coercion[tipe,term] => {.
    _coerce(Tp) => some(strg(encodeSignature(Tp))).
  .}
  
  implementation coercion[importSpec,term] => {.
    _coerce(pkgImp(_,Vz,Pk)) => some(term(tLbl("import",2),[Vz::term,Pk::term]))
  .}

  implementation coercion[canonDef,term] => {.
    _coerce(cnsDef(_,Nm,FullNm,Tp)) =>
      some(term(tLbl("constructor",3),[strg(Nm),strg(FullNm),Tp::term])).
    _coerce(conDef(_,Nm,FullNm,Tp)) =>
      some(term(tLbl("contract",3),[strg(Nm),strg(FullNm),Tp::term])).
  .}
  
  implementation all e ~~ coercion[e,term] |: coercion[cons[e],term] => {.
    _coerce(L)=>some(mkTpl(L//(e)=>e::term))
  .}

  implementation coercion[implSpec,term] => {.
    _coerce(implSpec(_,ConNm,FullNm,Spec)) =>
      some(term(tLbl("impl",3),[strg(ConNm),strg(FullNm),Spec::term]))
  .}
  
  public implementation coercion[pkgSpec,term] => let{
    mkTerm(pkgSpec(Pkg,Imports,Fields,Contracts,Implementations,_)) =>
      term(tLbl("pkgSpec",5),[Pkg::term,
	  Imports::term,strg(encodeSignature(Fields)),Contracts::term,Implementations::term]).
  } in {
    _coerce(S) => some(mkTerm(S)).
  }
}
