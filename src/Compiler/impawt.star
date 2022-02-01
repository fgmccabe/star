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
    if {? (PkgVar,_) in Sigs ?} then
      importAll(Imports,Repo,Env,Imported,Sigs,Rp)
    else{
      try{
	pkgSpec(_,PkgImps,Decls) <- importPkg(Pkg,Lc,Repo,Rp);
	
	E0 .= pushSig(Sig,Lc,
	  (Id,T,E) where (_,DQ).=deQuant(T)=> (_ ^= isConsType(DQ) ?
	    declareConstructor(Id,qualifiedName(pkgName(Pkg),.conMark,Id),some(Lc),T,E) ||
	      declareFldAccess(vr(Lc,PkgVar,Sig),Id,some(Lc),T,E)),Env);
	E1 .= foldRight((conDef(_,CNm,CFNm,CTp),EE)=>
	    declareContract(Lc,CNm,CTp,EE),E0,Cons);
	E2 .= foldRight((implSpec(ILc,ConNm,FullNm,Tp),EE)=>
	    declareFldAccess(vr(Lc,PkgVar,Sig),FullNm,some(Lc),Tp,
	      declareImplementation(Lc,FullNm,FullNm,Tp,EE)),E1,Impls);
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
    (term(_,[Pk,term(_,Imps),term(_,Decls)]),_)<-decodeTerm(Txt::cons[char]);
    Pkg <- pickupPkg(Pk);    
    Imports <- pickupImports(Imps,Lc);
    Decls <- pickupDeclarations(Decls,Lc,Rp);
    valis pkgSpec(Pkg,Imports,Decls)
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
  pickupImports(Trms,Lc) => let{.
    pickupImps([],Imx) => either(Imx).
    pickupImps([term(_,[V,P]),..Imps],Imx) where
	Vz ^= pickupViz(V) => do{
	  Pkg <- pickupPkg(P);
	  pickupImps(Imps,[pkgImp(Lc,Vz,Pkg),..Imx])
	}.
  .} in pickupImps(Trms,[]).

  pickupDeclarations:(cons[term],locn,reports)=>either[reports,cons[decl]].
  pickupDeclarations(Ts,Lc,Rp) => seqmap((T)=>pickupDeclaration(T,Lc,Rp),Ts).

  pickupDeclaration(term(tLbl("imp",3),[strg(Nm),strg(FNm),strg(Sig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      valis implDec(Nm,FNm,Tp)
    } catch{
      raise reportError(Rp,"invalid implementation type signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("acc",4),
      [strg(Sig),strg(Fld),strg(FNm),strg(AccSig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      AccTp <- decodeSignature(AccSig);
      valis accDec(Tp,Nm,FNm,AccTp)
    } catch{
      raise reportError(Rp,"invalid accessor signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("con",4),
      [strg(Nm),strg(CnNm),strg(TSig),strg(Sig)]),Lc,Rp) => do{
    try{
      ConTp <- decodeSignature(TSig);
      TpRl <- decodeSignature(Sig);
      valis conDec(Nm,CnNm,ConTp,TpRl)
    } catch{
      raise reportError(Rp,"invalid contract signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("tpe",3),
      [strg(Nm),strg(TSig),strg(RSig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(TSig);
      RlTp <- decodeSignature(RSig);
      valis tpeDec(Nm,Tp,RlTp)
    } catch{
      raise reportError(Rp,"invalid type signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("var",3),
      [strg(Nm),strg(FlNm),strg(Sig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      valis varDec(Nm,FlNm,Tp)
    } catch{
      raise reportError(Rp,"invalid var signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("fun",3),
      [strg(Nm),strg(FlNm),strg(Sig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      valis funDec(Nm,FlNm,Tp)
    } catch{
      raise reportError(Rp,"invalid function signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("cns",3),
      [strg(Nm),strg(FlNm),strg(Sig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      valis cnsDec(Nm,FlNm,Tp)
    } catch{
      raise reportError(Rp,"invalid constructor signature",Lc)
    }
  }
  pickupDeclaration(T,Lc,Rp) => do{
    raise reportError(Rp,"invalid declaration",Lc).


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

  implementation coercion[visibility,term] => {
    _coerce(.priVate) => some(strg("private")).
    _coerce(.pUblic) => some(strg("public")).
    _coerce(.transItive) => some(strg("transitive")).
  }

  implementation coercion[tipe,term] => {
    _coerce(Tp) => some(strg(encodeSignature(Tp))).
  }
  
  implementation coercion[importSpec,term] => {
    _coerce(pkgImp(_,Vz,Pk)) => some(term(tLbl("import",2),[Vz::term,Pk::term]))
  }

  implementation coercion[canonDef,term] => {
    _coerce(cnsDef(_,Nm,FullNm,Tp)) =>
      some(term(tLbl("constructor",3),[strg(Nm),strg(FullNm),Tp::term])).
    _coerce(conDef(_,Nm,FullNm,Tp)) =>
      some(term(tLbl("contract",3),[strg(Nm),strg(FullNm),Tp::term])).
  }
  
  implementation all e ~~ coercion[e,term] |: coercion[cons[e],term] => {
    _coerce(L)=>some(mkTpl(L//(e)=>e::term))
  }

  implementation coercion[implSpec,term] => {
    _coerce(implSpec(_,ConNm,FullNm,Spec)) =>
      some(term(tLbl("impl",3),[strg(ConNm),strg(FullNm),Spec::term]))
  }
  
  public implementation coercion[pkgSpec,term] => let{
    mkTerm(pkgSpec(Pkg,Imports,Decls)) =>
      term(tLbl("pkgSpec",3),[Pkg::term,Imports::term,Decls//(D)=>(D::term)]).
  } in {
    _coerce(S) => some(mkTerm(S)).
  }

  public implementation coercion[decl,term] => let{
    mkTerm(implDec(Nm,FullNm,Tp)) => term(tLbl("imp",3),[strg(Nm),strg(FullNm),Tp::term]).
    mkTerm(accDec(Tp,Fld,Acc,AccTp)) =>
      term(tLbl("acc",4),[Tp::term,strg(Fld),strg(Acc),AccTp::term]).
    mkTerm(conDec(Nm,FlNm,CnTp,CtRl)) =>
      term(tLbl("con",4),[strg(Nm),strg(FlNm),CnTp::term,CtRl::term]).
    mkTerm(tpeDec(Nm,Tp,Rl)) =>
      term(tLbl("tpe",3),[strg(Nm),Tp::term,Rl::term]).
    mkTerm(varDec(Nm,FlNm,Tp)) =>
      term(tLbl("var",3),[strg(Nm),strg(FlNm),Tp::term]).
    mkTerm(funDec(Nm,FlNm,Tp)) =>
      term(tLbl("fun",3),[strg(Nm),strg(FlNm),Tp::term]).
    mkTerm(cnsDec(Nm,FlNm,Tp)) =>
      term(tLbl("cns",3),[strg(Nm),strg(FlNm),Tp::term]).
  } in {
    _coerce(D) => some(mkTerm(D)).
  }
}
