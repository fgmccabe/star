star.compiler.impawt{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.meta.
  import star.compiler.data.
  import star.compiler.types.

  public importAll:all r ~~ repo[r]|:
    (cons[importSpec],r,cons[importSpec], cons[decl]) => (cons[importSpec],cons[decl]).
  importAll([],_,Imported,Decls) => (Imported,Decls).
  importAll([.pkgImp(Lc,Viz,Pkg),..Imports],Repo,Imported,Decls) => valof{
    if {? .pkgImp(_,_,Pkg) in Imported ?} then
      valis importAll(Imports,Repo,Imported,Decls)
    else if .pkgSpec(_,PkgImps,PDecls) ?= importPkg(Pkg,Lc,Repo) then{
      valis importAll(Imports++PkgImps,Repo,[.pkgImp(Lc,Viz,Pkg),..Imported],
	Decls++PDecls)
    }
    else {
      reportError("cannot import $(Pkg)",Lc);
      valis (Imports,Decls)
    }
  }

  public importPkg:all r ~~ repo[r] |: (pkg,option[locn],r) => option[pkgSpec].
  importPkg(Pkg,Lc,Repo) where Sig ?= pkgSignature(Repo,Pkg) => 
    pickupPkgSpec(Sig,Lc).
  importPkg(Pkg,Lc,_) default => .none.

  pickupPkgSpec:(string,option[locn]) => option[pkgSpec].
  pickupPkgSpec(Txt,Lc) => valof{
    if (.term(_,[Pk,.term(_,Imps),.term(_,Ds)]),_).=decodeTerm(Txt::cons[char]) then{
      Pkg = ^pickupPkg(Pk);
      Imports = pickupImports(Imps,Lc);
      Decls = pickupDeclarations(Ds,Lc);
      valis .some(pkgSpec(Pkg,Imports,Decls))
    } else {
      reportError("count not decode package spec",Lc);
      valis .none
    }
  }

  pickupPkg:(data) => option[pkg].
  pickupPkg(.term(.tLbl("pkg",2),[.strg(Nm),V])) where Ver?=pickupVersion(V) =>
    .some(.pkg(Nm,Ver)).
  pickupPkg(_) default => .none.

  pickupVersion:(data)=>option[version].
  pickupVersion(.symb(.tLbl("*",0))) => .some(.defltVersion).
  pickupVersion(.strg(V)) => .some(.vers(V)).
  pickupVersion(_) default => .none.

  pickupViz:(data)=>option[visibility].
  pickupViz(.strg("private")) => .some(.priVate).
  pickupViz(.strg("public")) => .some(.pUblic).
  pickupViz(.strg("transitive")) => .some(.transItive).
  pickupViz(.symb(.tLbl("private",0))) => .some(.priVate).
  pickupViz(.symb(.tLbl("public",0))) => .some(.pUblic).
  pickupViz(.symb(.tLbl("transitive",0))) => .some(.transItive).
  pickupViz(_) default => .none.

  pickupImports:(cons[data],option[locn]) => cons[importSpec].
  pickupImports(Trms,Lc) => let{.
    pickupImps([],Imx) => Imx.
    pickupImps([.term(O,[V,P]),..Imps],Imx) => valof{
      if Pkg?=pickupPkg(P) && Vz ?= pickupViz(V) then
	valis pickupImps(Imps,[.pkgImp(Lc,Vz,Pkg),..Imx])
      else{
	reportError("Ignoring invalid pkg import spec $(term(O,[V,P]))",Lc);
	valis pickupImps(Imps,Imx)
      }
    }
  .} in pickupImps(Trms,[]).

  pickupDeclarations:(cons[data],option[locn])=>cons[decl].
  pickupDeclarations([],_Lc) => [].
  pickupDeclarations([T,..Ts],Lc) => (
    D ?= pickupDeclaration(T,Lc) ?
      [D,..pickupDeclarations(Ts,Lc)] ||
      pickupDeclarations(Ts,Lc)).
	
  pickupDeclaration:(data,option[locn])=>option[decl].
  pickupDeclaration(.term(.tLbl("imp",3),[.strg(Nm),.strg(FNm),.strg(Sig)]),Lc) => valof{
    try{
      valis .some(.implDec(Lc,Nm,FNm,decodeSignature(Sig)))
    } catch{
      _ => {
	reportError("invalid implementation type signature",Lc);
	valis .none
      }
    }
  }
  pickupDeclaration(.term(.tLbl("acc",4),
      [.strg(Sig),.strg(Fld),.strg(FNm),.strg(AccSig)]),Lc) => valof{
    try{
      Tp = decodeSignature(Sig);
      AccTp = decodeSignature(AccSig);
      valis .some(.accDec(Lc,Tp,Fld,FNm,AccTp))
    } catch{
      _ => {
	reportError("invalid accessor signature",Lc);
	valis .none
      }
    }
  }
  pickupDeclaration(.term(.tLbl("upd",4),
      [.strg(Sig),.strg(Fld),.strg(FNm),.strg(AccSig)]),Lc) => valof{
    try{
      Tp = decodeSignature(Sig);
      AccTp = decodeSignature(AccSig);
      valis .some(.updDec(Lc,Tp,Fld,FNm,AccTp))
    } catch{
      _ => {
	reportError("invalid updater signature",Lc);
	valis .none
      }
    }
  }
  pickupDeclaration(.term(.tLbl("con",3),
      [.strg(Nm),.strg(CnNm),.strg(Sig)]),Lc) => valof{
    try{
      valis .some(.conDec(Lc,Nm,CnNm,decodeTypeRuleSignature(Sig)))
    } catch{
      _ => {
	reportError("invalid contract signature",Lc);
	valis .none
      }
    }
  }
  pickupDeclaration(.term(.tLbl("tpe",3),
      [.strg(Nm),.strg(TSig),.strg(RSig)]),Lc) => valof{
    try{
      Tp = decodeSignature(TSig);
      RlTp = decodeTypeRuleSignature(RSig);
      valis .some(.tpeDec(Lc,Nm,Tp,RlTp))
    } catch{
      _ => {
	reportError("invalid type signature",Lc);
	valis .none
      }
    }
  }
  pickupDeclaration(.term(.tLbl("var",3),
      [.strg(Nm),.strg(FlNm),.strg(Sig)]),Lc) => valof{
    try{
      valis .some(.varDec(Lc,Nm,FlNm,decodeSignature(Sig)))
    } catch{
      _ => {
	reportError("invalid var signature",Lc);
	valis .none
      }
    }
  }

  pickupDeclaration(.term(.tLbl("fun",3),
      [.strg(Nm),.strg(FlNm),.strg(Sig)]),Lc) => valof{
    try{
      valis .some(.funDec(Lc,Nm,FlNm,decodeSignature(Sig)))
    } catch{
      _ => {
	reportError("invalid function signature",Lc);
	valis .none
      }
    }
  }

  pickupDeclaration(.term(.tLbl("cns",3),
      [.strg(Nm),.strg(FlNm),.strg(Sig)]),Lc) => valof{
    try{
      valis .some(.cnsDec(Lc,Nm,FlNm,decodeSignature(Sig)))
    } catch{
      _ => {
	reportError("invalid constructor signature",Lc);
	valis .none
      }
    }
  }
  pickupDeclaration(T,Lc) => valof{
    reportError("invalid declaration",Lc);
    valis .none
  }

  implementation coercion[pkg,data] => {
    _coerce(.pkg(P,.defltVersion)) => .some(.term(.tLbl("pkg",2),[.strg(P),.strg("*")])).
    _coerce(.pkg(P,.vers(V))) => .some(.term(.tLbl("pkg",2),[.strg(P),.strg(V)])).
  }

  implementation coercion[visibility,data] => {
    _coerce(.priVate) => .some(.strg("private")).
    _coerce(.pUblic) => .some(.strg("public")).
    _coerce(.transItive) => .some(.strg("transitive")).
  }

  implementation coercion[tipe,data] => {
    _coerce(Tp) => .some(.strg(encodeSignature(Tp))).
  }

  implementation coercion[typeRule,data] => {
    _coerce(Rl) => .some(.strg(encodeTpRlSignature(Rl))).
  }
  
  implementation coercion[importSpec,data] => {
    _coerce(.pkgImp(_,Vz,Pk)) => .some(.term(.tLbl("import",2),[Vz::data,Pk::data]))
  }

  implementation coercion[canonDef,data] => {
    _coerce(.cnsDef(_,Nm,FullNm,Tp)) =>
      .some(.term(.tLbl("constructor",3),[.strg(Nm),.strg(FullNm),Tp::data])).
    _coerce(.conDef(_,Nm,FullNm,Tp)) =>
      .some(.term(.tLbl("contract",3),[.strg(Nm),.strg(FullNm),Tp::data])).
  }
  
  implementation all e ~~ coercion[e,data] |: coercion[cons[e],data] => {
    _coerce(L)=>.some(mkTpl(L//(e)=>e::data))
  }

  public implementation coercion[pkgSpec,data] => let{
    mkTerm(.pkgSpec(Pkg,Imports,Decls)) =>
      .term(.tLbl("pkgSpec",3),[Pkg::data,Imports::data,mkTpl(Decls//(D)=>(D::data))]).
  } in {
    _coerce(S) => .some(mkTerm(S)).
  }

  public implementation coercion[decl,data] => let{
    mkTerm(.implDec(_,Nm,FullNm,Tp)) =>
      .term(.tLbl("imp",3),[.strg(Nm),.strg(FullNm),Tp::data]).
    mkTerm(.accDec(_,Tp,Fld,Acc,AccTp)) =>
      .term(.tLbl("acc",4),[Tp::data,.strg(Fld),.strg(Acc),AccTp::data]).
    mkTerm(.updDec(_,Tp,Fld,Acc,AccTp)) =>
      .term(.tLbl("upd",4),[Tp::data,.strg(Fld),.strg(Acc),AccTp::data]).
    mkTerm(.conDec(_,Nm,FlNm,CtRl)) =>
      .term(.tLbl("con",3),[.strg(Nm),.strg(FlNm),CtRl::data]).
    mkTerm(.tpeDec(_,Nm,Tp,Rl)) =>
      .term(.tLbl("tpe",3),[.strg(Nm),Tp::data,Rl::data]).
    mkTerm(.varDec(_,Nm,FlNm,Tp)) =>
      .term(.tLbl("var",3),[.strg(Nm),.strg(FlNm),Tp::data]).
    mkTerm(.funDec(_,Nm,FlNm,Tp)) =>
      .term(.tLbl("fun",3),[.strg(Nm),.strg(FlNm),Tp::data]).
    mkTerm(.cnsDec(_,Nm,FlNm,Tp)) =>
      .term(.tLbl("cns",3),[.strg(Nm),.strg(FlNm),Tp::data]).
  } in {
    _coerce(D) => .some(mkTerm(D)).
  }
}
