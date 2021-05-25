:- module(gensig,[genPkgSig/2,encPkg/2]).

:- use_module(misc).
:- use_module(lterms).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genPkgSig(mdule(Pkg,Imports,PkgTp,Face,Enums,_Defs,Contracts,Impls),Sig) :-
  constructPkgSig(Pkg,Imports,PkgTp,Face,Enums,Contracts,Impls,Term),
  encodeTerm(Term,Chrs,[]),
  string_chars(Sig,Chrs).

constructPkgSig(Pkg,Imports,PkgTp,Face,Enums,Contracts,Impls,SigTpl) :-
  mkTpl([PkgNm,ImpTpl,strg(PkSig),strg(Sig),ClsTpl,ConTpl,ImplTpl],SigTpl),
  encPkg(Pkg,PkgNm),
  encImports(Imports,Imps),
  mkTpl(Imps,ImpTpl),
  encType(PkgTp,PkSig),
  encType(Face,Sig),
  map(Enums,gensig:formatCnMap,ClsSigs),
  mkTpl(ClsSigs,ClsTpl),
  formatContracts(Contracts,ConSigs),
  mkTpl(ConSigs,ConTpl),
  formatImpls(Impls,ImplSigs),
  mkTpl(ImplSigs,ImplTpl).

encPkg(pkg(Nm,Vers),ctpl(lbl("pkg",2),[strg(Nm),V])) :-
  encVer(Vers,V).

encVer(defltVersion,enum("*")).
encVer(ver(V),strg(V)).

encImports([],[]).
encImports([I|M],[IP|L]) :-
  encImport(I,IP),
  encImports(M,L).

encImport(import(Viz,Pkg,_,_,_,_,_),ctpl(lbl("import",2),[enum(Viz),Enc])) :-
  encPkg(Pkg,Enc).

formatCnMap((TpNm,Cns),ConTpl) :-
  map(Cns,gensig:formatCns,Cnx),
  mkTpl(Cnx,CnTpl),
  mkTpl([strg(TpNm),CnTpl],ConTpl).

formatCns((Nm,FlNm,Tp),Fmt) :-
  encodeSignature(Tp,CnSig),
  mkTpl([strg(Nm),strg(FlNm),CnSig],Fmt).

formatContracts([],[]).
formatContracts([conDef(Lc,Nm,CnNm,CnTp,Spec)|M],[ConTpl|R]) :-
  locTerm(Lc,LTrm),
  encodeSignature(Spec,SpecSig),
  encodeSignature(CnTp,CnSig),
  mkTpl([LTrm,strg(Nm),strg(CnNm),CnSig,SpecSig],ConTpl),
  formatContracts(M,R).

formatImpls([],[]).
formatImpls([imp(Nm,FullNm,Spec)|M],[ctpl(lbl("imp",3),ImplTpl)|R]) :-
  encodeSignature(Spec,SpecSig),
  ImplTpl=[strg(Nm),strg(FullNm),SpecSig],
  formatImpls(M,R).
formatImpls([acc(Tp,Fld,AccFn,AccTp)|M],[ctpl(lbl("acc",3),ImplTpl)|R]) :-
  encodeSignature(Tp,TpSig),
  encodeSignature(AccTp,AccSig),
  ImplTpl=[TpSig,strg(Fld),strg(AccFn),AccSig],
  formatImpls(M,R).

encodeSignature(Tp,strg(Sig)) :-
  encodeType(Tp,Chars,[]),
  string_chars(Sig,Chars).
  
