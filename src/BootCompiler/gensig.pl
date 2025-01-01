:- module(gensig,[genPkgSig/2,encPkg/2,formatDecl/2,
		  encodeSignature/2]).

:- use_module(misc).
:- use_module(lterms).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genPkgSig(mdule(Pkg,Imports,Decls,_,_),Sig) :-
  constructPkgSig(Pkg,Imports,Decls,Term),
  encodeTerm(Term,Chrs,[]),!,
  string_chars(Sig,Chrs).

constructPkgSig(Pkg,Imports,Decls,SigTpl) :-
  mkTpl([PkgNm,ImpTpl,DeclTpl],SigTpl),
  encPkg(Pkg,PkgNm),
  encImports(Imports,Imps),
  mkTpl(Imps,ImpTpl),
  map(Decls,gensig:formatDecl,DeclSigs),
  mkTpl(DeclSigs,DeclTpl).

encPkg(pkg(Nm,Vers),ctpl(lbl("pkg",2),[strg(Nm),V])) :-
  encVer(Vers,V).

encVer(defltVersion,enum("*")).
encVer(ver(V),strg(V)).

encImports([],[]).
encImports([I|M],[IP|L]) :-
  encImport(I,IP),
  encImports(M,L).

encImport(importPk(_,Viz,Pkg),ctpl(lbl("import",2),[enum(Viz),Enc])) :-
  encPkg(Pkg,Enc).

formatDecl(varDec(Nm,FullNm,Tp),ctpl(lbl("var",3),[strg(Nm),strg(FullNm),Sig])) :-
  encodeSignature(Tp,Sig).
formatDecl(funDec(Nm,FullNm,Tp),ctpl(lbl("fun",3),[strg(Nm),strg(FullNm),Sig])) :-
  encodeSignature(Tp,Sig).
formatDecl(typeDec(Nm,Tp,TpRule),ctpl(lbl("tpe",3),[strg(Nm),TpSig,RlSig])) :-
  encodeSignature(Tp,TpSig),
  encodeSignature(TpRule,RlSig).
formatDecl(accDec(Tp,Fld,Fn,AccTp),
	   ctpl(lbl("acc",4),[TpSig,strg(Fld),strg(Fn),AccSig])) :-
  encodeSignature(Tp,TpSig),
  encodeSignature(AccTp,AccSig).
formatDecl(updDec(Tp,Fld,Fn,AccTp),
	   ctpl(lbl("upd",4),[TpSig,strg(Fld),strg(Fn),AccSig])) :-
  encodeSignature(Tp,TpSig),
  encodeSignature(AccTp,AccSig).
formatDecl(impDec(ConNm,ImplNm,ImplTp),
	   ctpl(lbl("imp",3),[strg(ConNm),strg(ImplNm),ImplSig])) :-
  encodeSignature(ImplTp,ImplSig).
formatDecl(contractDec(Nm,CnNm,Spec),
	   ctpl(lbl("con",3),[strg(Nm),strg(CnNm),SpecSig])) :-
  encodeSignature(Spec,SpecSig).
formatDecl(cnsDec(Nm,FullNm,Tp),ctpl(lbl("cns",3),
				     [strg(Nm),strg(FullNm),Sig])) :-
  encodeSignature(Tp,Sig).


formatCnMap((TpNm,Cns),ConTpl) :-
  map(Cns,gensig:formatCns,Cnx),
  mkTpl(Cnx,CnTpl),
  mkTpl([strg(TpNm),CnTpl],ConTpl).

formatCns((Nm,FlNm,Tp),Fmt) :-
  encodeSignature(Tp,CnSig),
  mkTpl([strg(Nm),strg(FlNm),CnSig],Fmt).

formatContracts([],[]).
formatContracts([conDef(Nm,CnNm,Spec)|M],[ConTpl|R]) :-
  encodeSignature(Spec,SpecSig),
  mkTpl([strg(Nm),strg(CnNm),SpecSig],ConTpl),
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
formatImpls([upd(Tp,Fld,AccFn,AccTp)|M],[ctpl(lbl("upd",3),ImplTpl)|R]) :-
  encodeSignature(Tp,TpSig),
  encodeSignature(AccTp,AccSig),
  ImplTpl=[TpSig,strg(Fld),strg(AccFn),AccSig],
  formatImpls(M,R).

encodeSignature(Tp,strg(Sig)) :-
  encodeType(Tp,Chars,[]),
  string_chars(Sig,Chars).
  
