star.compiler.impawt{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.meta.
  import star.compiler.terms.
  import star.compiler.types.

  public importAll:all r ~~ repo[r]|:
    (cons[importSpec],r,cons[importSpec],
      cons[decl],reports) => either[reports,(cons[importSpec],cons[decl])].
  importAll([],_,Imported,Decls,_) => either((Imported,Decls)).
  importAll([pkgImp(Lc,Viz,Pkg),..Imports],Repo,Imported,Decls,Rp) => do{
    if {? pkgImp(_,_,Pkg) in Imported ?} then
      importAll(Imports,Repo,Imported,Decls,Rp)
    else{
      try{
	pkgSpec(_,PkgImps,PDecls) <- importPkg(Pkg,Lc,Repo,Rp);
	importAll(Imports++PkgImps,Repo,[pkgImp(Lc,Viz,Pkg),..Imported],
	  Decls++PDecls,Rp)
      }
      catch {
	raise reportError(Rp,"cannot import $(Pkg)",Lc)
      }
    }
  }

  public importPkg:all r ~~ repo[r] |: (pkg,option[locn],r,reports) => either[reports,pkgSpec].
  importPkg(Pkg,Lc,Repo,Rp) where Sig ^= pkgSignature(Repo,Pkg) => either (valof pickupPkgSpec(Sig,Lc,Rp)).
  importPkg(Pkg,Lc,_,Rp) default => other(reportError(Rp,"cannot import $(Pkg)",Lc)).

  pickupPkgSpec:(string,option[locn],reports) => either[reports,pkgSpec].
  pickupPkgSpec(Txt,Lc,Rp) => do{
    try{
      (term(_,[Pk,term(_,Imps),term(_,Ds)]),_)<-decodeTerm(Txt::cons[char]);
      Pkg <- pickupPkg(Pk);
      try{
	Imports <- pickupImports(Imps,Lc);
	Decls <- pickupDeclarations(Ds,Lc,Rp);
	valis pkgSpec(Pkg,Imports,Decls)
      } catch{
	raise ()
      }
    }
    catch{
      raise reportError(Rp,"count not decode package spec",Lc)
    }
  }

  pickupPkg:all e ~~ (term) => either[e,pkg].
  pickupPkg(term(tLbl("pkg",2),[strg(Nm),V])) => do{
    Vr <- pickupVersion(V);
    valis pkg(Nm,Vr)
  }

  pickupVersion:all e ~~ (term)=>either[e,version].
  pickupVersion(symb(tLbl("*",0))) => either(.defltVersion).
  pickupVersion(strg(V)) => either(vers(V)).

  pickupViz:(term)=>option[visibility].
  pickupViz(symb(tLbl("private",0))) => some(.priVate).
  pickupViz(symb(tLbl("public",0))) => some(.pUblic).
  pickupViz(symb(tLbl("transitive",0))) => some(.transItive).
  pickupVis(_) default => .none.

  pickupImports:(cons[term],option[locn]) => either[reports,cons[importSpec]].
  pickupImports(Trms,Lc) => let{.
    pickupImps([],Imx) => either(Imx).
    pickupImps([term(_,[V,P]),..Imps],Imx) where
	Vz ^= pickupViz(V) => do{
	  Pkg <- pickupPkg(P);
	  pickupImps(Imps,[pkgImp(Lc,Vz,Pkg),..Imx])
	}.
  .} in pickupImps(Trms,[]).

  pickupDeclarations:(cons[term],option[locn],reports)=>either[reports,cons[decl]].
  pickupDeclarations(Ts,Lc,Rp) => seqmap((T)=>pickupDeclaration(T,Lc,Rp),Ts).

  pickupDeclaration(term(tLbl("imp",3),[strg(Nm),strg(FNm),strg(Sig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      valis implDec(Lc,Nm,FNm,Tp)
    } catch{
      raise reportError(Rp,"invalid implementation type signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("acc",4),
      [strg(Sig),strg(Fld),strg(FNm),strg(AccSig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      AccTp <- decodeSignature(AccSig);
      valis accDec(Lc,Tp,Fld,FNm,AccTp)
    } catch{
      raise reportError(Rp,"invalid accessor signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("upd",4),
      [strg(Sig),strg(Fld),strg(FNm),strg(AccSig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      AccTp <- decodeSignature(AccSig);
      valis updDec(Lc,Tp,Fld,FNm,AccTp)
    } catch{
      raise reportError(Rp,"invalid updater signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("con",3),
      [strg(Nm),strg(CnNm),strg(Sig)]),Lc,Rp) => do{
    try{
      TpRl <- decodeTypeRuleSignature(Sig);
      valis conDec(Lc,Nm,CnNm,TpRl)
    } catch{
      raise reportError(Rp,"invalid contract signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("tpe",3),
      [strg(Nm),strg(TSig),strg(RSig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(TSig);
      RlTp <- decodeTypeRuleSignature(RSig);
      valis tpeDec(Lc,Nm,Tp,RlTp)
    } catch{
      raise reportError(Rp,"invalid type signature",Lc)
    }
  }
  pickupDeclaration(term(tLbl("var",3),
      [strg(Nm),strg(FlNm),strg(Sig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      valis varDec(Lc,Nm,FlNm,Tp)
    } catch{
      raise reportError(Rp,"invalid var signature",Lc)
    }
  }

  pickupDeclaration(term(tLbl("fun",3),
      [strg(Nm),strg(FlNm),strg(Sig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      valis funDec(Lc,Nm,FlNm,Tp)
    } catch{
      raise reportError(Rp,"invalid function signature",Lc)
    }
  }

  pickupDeclaration(term(tLbl("cns",3),
      [strg(Nm),strg(FlNm),strg(Sig)]),Lc,Rp) => do{
    try{
      Tp <- decodeSignature(Sig);
      valis cnsDec(Lc,Nm,FlNm,Tp)
    } catch{
      raise reportError(Rp,"invalid constructor signature",Lc)
    }
  }

  pickupDeclaration(T,Lc,Rp) => do{
    raise reportError(Rp,"invalid declaration",Lc).
  }

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

  implementation coercion[typeRule,term] => {
    _coerce(Rl) => some(strg(encodeTpRlSignature(Rl))).
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

  public implementation coercion[pkgSpec,term] => let{
    mkTerm(pkgSpec(Pkg,Imports,Decls)) =>
      term(tLbl("pkgSpec",3),[Pkg::term,Imports::term,mkTpl(Decls//(D)=>(D::term))]).
  } in {
    _coerce(S) => some(mkTerm(S)).
  }

  public implementation coercion[decl,term] => let{
    mkTerm(implDec(_,Nm,FullNm,Tp)) => term(tLbl("imp",3),[strg(Nm),strg(FullNm),Tp::term]).
    mkTerm(accDec(_,Tp,Fld,Acc,AccTp)) =>
      term(tLbl("acc",4),[Tp::term,strg(Fld),strg(Acc),AccTp::term]).
    mkTerm(updDec(_,Tp,Fld,Acc,AccTp)) =>
      term(tLbl("upd",4),[Tp::term,strg(Fld),strg(Acc),AccTp::term]).
    mkTerm(conDec(_,Nm,FlNm,CtRl)) =>
      term(tLbl("con",3),[strg(Nm),strg(FlNm),CtRl::term]).
    mkTerm(tpeDec(_,Nm,Tp,Rl)) =>
      term(tLbl("tpe",3),[strg(Nm),Tp::term,Rl::term]).
    mkTerm(varDec(_,Nm,FlNm,Tp)) =>
      term(tLbl("var",3),[strg(Nm),strg(FlNm),Tp::term]).
    mkTerm(funDec(_,Nm,FlNm,Tp)) =>
      term(tLbl("fun",3),[strg(Nm),strg(FlNm),Tp::term]).
    mkTerm(cnsDec(_,Nm,FlNm,Tp)) =>
      term(tLbl("cns",3),[strg(Nm),strg(FlNm),Tp::term]).
  } in {
    _coerce(D) => some(mkTerm(D)).
  }
}
