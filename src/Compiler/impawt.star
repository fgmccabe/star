star.compiler.impawt{
  import star.

  import star.pkg.
  import star.repo.

  import star.compiler.term.repo.
  import star.compiler.canon.
  import star.compiler.errors.
  import star.compiler.location.
  import star.compiler.misc.
  import star.compiler.meta.
  import star.compiler.data.
  import star.compiler.term.
  import star.compiler.types.

  public importAll:all r ~~ repo[r]|:
    (cons[importSpec],r,cons[importSpec], cons[decl]) => (cons[importSpec],cons[decl]).
  importAll([],_,Imported,Decls) => (Imported,Decls).
  importAll([.pkgImp(Lc,Viz,Pkg),..Imports],Repo,Imported,Decls) => valof{
    if {? .pkgImp(_,_,Pkg) in Imported ?} then
      valis importAll(Imports,Repo,Imported,Decls)
    else if P ?= importPkg(Pkg,Lc,Repo) then{
      valis importAll(Imports++P.imports,Repo,[.pkgImp(Lc,Viz,Pkg),..Imported],
	Decls++P.exports)
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
      valis ? pkgSpec{pkg=Pkg. imports=Imports. exports=Decls}
    } else {
      reportError("could not decode package spec",Lc);
      valis .none
    }
  }

  pickupPkg:(data) => option[pkg].
  pickupPkg(.term("pkg",[.strg(Nm),V])) where Ver?=pickupVersion(V) =>
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
  pickupDeclaration(.term("imp",[.strg(Nm),.strg(FNm),Sig]),Lc) => valof{
    try{
      valis .some(.implDec(Lc,Nm,FNm,decodeSig(Sig)))
    } catch{
      _ => {
	reportError("invalid implementation type signature",Lc);
	valis .none
      }
    }
  }
  pickupDeclaration(.term("acc",
      [Sig,.strg(Fld),.strg(FNm),AccSig]),Lc) => valof{
    try{
      Tp = decodeSig(Sig);
      AccTp = decodeSig(AccSig);
      valis .some(.accDec(Lc,Tp,Fld,FNm,AccTp))
    } catch{
      _ => {
	reportError("invalid accessor signature",Lc);
	valis .none
      }
    }
  }
  pickupDeclaration(.term("upd",
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
  pickupDeclaration(.term("con",
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
  pickupDeclaration(.term("tpe",
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
  pickupDeclaration(.term("var",
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

  pickupDeclaration(.term("fun",
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

  pickupDeclaration(.term("cns",
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
    _coerce(.pkg(P,.defltVersion)) => .some(.term("pkg",[.strg(P),.symb(.tLbl("*",0))])).
    _coerce(.pkg(P,.vers(V))) => .some(.term("pkg",[.strg(P),.strg(V)])).
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
    _coerce(.pkgImp(_,Vz,Pk)) => .some(.term("import",[Vz::data,Pk::data]))
  }

  implementation coercion[canonDef,data] => {
    _coerce(.cnsDef(_,Nm,FullNm,Tp)) =>
      .some(.term("constructor",[.strg(Nm),.strg(FullNm),Tp::data])).
    _coerce(.conDef(_,Nm,FullNm,Tp)) =>
      .some(.term("contract",[.strg(Nm),.strg(FullNm),Tp::data])).
  }
  
  implementation all e ~~ coercion[e,data] |: coercion[cons[e],data] => {
    _coerce(L)=>.some(mkTpl(L//(e)=>e::data))
  }

  public implementation coercion[pkgSpec,data] => let{
    mkTerm(pkgSpec{pkg=Pkg. imports=Imports. exports=Decls}) =>
      .term("pkgSpec",[Pkg::data,Imports::data,mkTpl(Decls//(D)=>(D::data))]).
  } in {
    _coerce(S) => .some(mkTerm(S)).
  }

  public implementation coercion[decl,data] => let{
    mkTerm(.implDec(_,Nm,FullNm,Tp)) =>
      .term("imp",[.strg(Nm),.strg(FullNm),Tp::data]).
    mkTerm(.accDec(_,Tp,Fld,Acc,AccTp)) =>
      .term("acc",[Tp::data,.strg(Fld),.strg(Acc),AccTp::data]).
    mkTerm(.updDec(_,Tp,Fld,Acc,AccTp)) =>
      .term("upd",[Tp::data,.strg(Fld),.strg(Acc),AccTp::data]).
    mkTerm(.conDec(_,Nm,FlNm,CtRl)) =>
      .term("con",[.strg(Nm),.strg(FlNm),CtRl::data]).
    mkTerm(.tpeDec(_,Nm,Tp,Rl)) =>
      .term("tpe",[.strg(Nm),Tp::data,Rl::data]).
    mkTerm(.varDec(_,Nm,FlNm,Tp)) =>
      .term("var",[.strg(Nm),.strg(FlNm),Tp::data]).
    mkTerm(.funDec(_,Nm,FlNm,Tp)) =>
      .term("fun",[.strg(Nm),.strg(FlNm),Tp::data]).
    mkTerm(.cnsDec(_,Nm,FlNm,Tp)) =>
      .term("cns",[.strg(Nm),.strg(FlNm),Tp::data]).
  } in {
    _coerce(D) => .some(mkTerm(D)).
  }

  public importLowered:(pkg,termRepo) => option[cons[cDefn]].
  importLowered(Pkg,R) => valof{
    if Txt ?= packageLowered(R,Pkg) then{
      if .term(_,Dta) ?= Txt:?data then{
	valis ? (Dta//(D)=>thawDefn(D));
      }
    };
      
    valis .none
  }

  pickupPkgSpec:(string,option[locn]) => option[pkgSpec].
  pickupPkgSpec(Txt,Lc) => valof{
    if (.term(_,[Pk,.term(_,Imps),.term(_,Ds)]),_).=decodeTerm(Txt::cons[char]) then{
      Pkg = ^pickupPkg(Pk);
      Imports = pickupImports(Imps,Lc);
      Decls = pickupDeclarations(Ds,Lc);
      valis ? pkgSpec{pkg=Pkg. imports=Imports. exports=Decls}
    } else {
      reportError("count not decode package spec",Lc);
      valis .none
    }
  }

}
