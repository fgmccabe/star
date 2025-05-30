:- module(import, [importAll/3,importPkg/3]).


:- use_module(resource).
:- use_module(dict).
:- use_module(errors).
:- use_module(lterms).
:- use_module(types).
:- use_module(misc).
:- use_module(uri).
:- use_module(transitive).
:- use_module(transutils).
:- use_module(decode).
:- use_module(repository).

importAll(Imports,Repo,AllImports) :-
  closure(Imports,[],import:notAlreadyImported,import:importMore(Repo),AllImports),!.

importMore(Repo,importPk(Lc,Viz,Pkg),SoFar,[importPk(Lc,Viz,Pkg)|SoFar],Inp,More) :-
  importPkg(Pkg,Repo,spec(_,Imports,_)),
  addPublicImports(Imports,Lc,Inp,More),!.
importMore(_,importPk(Lc,_,Pkg),SoFar,SoFar,Inp,Inp) :-
  reportError("could not import package %s",[Pkg],Lc).

notAlreadyImported(importPk(_,_,Pkg),SoFar) :-
  \+ is_member(importPk(_,_,Pkg),SoFar),!.

addPublicImports([],_,Imp,Imp).
addPublicImports([import(public,Pkg)|I],Lc,Rest,
		 [importPk(Lc,transitive,Pkg)|Out]) :-
  addPublicImports(I,Lc,Rest,Out).
addPublicImports([import(private,_)|I],Lc,Rest,Out) :-
  addPublicImports(I,Lc,Rest,Out).
addPublicImports([import(transitive,_)|I],Lc,Rest,Out) :-
  addPublicImports(I,Lc,Rest,Out).

importPkg(Pkg,Repo,Spec) :-
  codePackagePresent(Repo,Pkg,_Act,Sig,_U,_SrcWhen,_When),
  pickupPkgSpec(Sig,Spec),!.

pickupPkgSpec(Enc,spec(Pkg,Imports,Decls)) :-
  decodeValue(Enc,ctpl(_,[Pk,ctpl(_,Imps),ctpl(_,DeclSigs)])),
  pickupPkg(Pk,Pkg),
  pickupImports(Imps,Imports),
  pickupDeclarations(DeclSigs,Decls).

pickupPkg(ctpl(lbl("pkg",2),[strg(Nm),V]),pkg(Nm,VV)) :-
  pickupVersion(V,VV).

pickupVersion(enum("*"),defltVersion).
pickupVersion(strg(V),ver(V)).

consistentVersion(ver(V),ver(V)).
consistentVersion(defltVersion,_).

pickupImports([],[]).
pickupImports([ctpl(lbl("import",2),[V,P])|L],[import(Viz,Pkg)|M]) :-
  pickupViz(V,Viz),
  pickupPkg(P,Pkg),
  pickupImports(L,M).

pickupViz(enum("private"),private).
pickupViz(enum("public"),public).
pickupViz(enum("transitive"),transitive).

pickupType(strg(Sig),Type) :-
  decodeSignature(Sig,Type).

pickupDeclarations(Encs,Impls) :-
  map(Encs,import:pickupDeclaration,Impls).

pickupDeclaration(ctpl(lbl("imp",3),[strg(Nm),strg(FNm),strg(Sig)]),
		  impDec(Nm,FNm,Spec)) :-
  decodeSignature(Sig,Spec).
pickupDeclaration(ctpl(lbl("acc",4),
		       [strg(Sig),strg(Fld),strg(Fn),strg(AccSig)]),
		  accDec(Tp,Fld,Fn,AccTp)) :-
  decodeSignature(Sig,Tp),
  decodeSignature(AccSig,AccTp).
pickupDeclaration(ctpl(lbl("upd",4),
		       [strg(Sig),strg(Fld),strg(Fn),strg(AccSig)]),
		  updDec(Tp,Fld,Fn,AccTp)) :-
  decodeSignature(Sig,Tp),
  decodeSignature(AccSig,AccTp).
pickupDeclaration(ctpl(lbl("con",3),
		       [strg(Nm),strg(CnNm),strg(Sig)]),
		  contractDec(Nm,CnNm,Spec)) :-
  decodeSignature(Sig,Spec).
pickupDeclaration(ctpl(lbl("tpe",4),[strg(Nm),strg(Sig),strg(RlSig),Mp]),
		  typeDec(Nm,Tp,TpRule,IxMap)) :-
  decodeSignature(Sig,Tp),
  decodeSignature(RlSig,TpRule),
  decodeConsMap(Mp,IxMap).
pickupDeclaration(ctpl(lbl("var",3),[strg(Nm),strg(FullNm),strg(Sig)]),
		  varDec(Nm,FullNm,Tp)) :-
  decodeSignature(Sig,Tp).
pickupDeclaration(ctpl(lbl("fun",3),[strg(Nm),strg(FullNm),strg(Sig)]),
		  funDec(Nm,FullNm,Tp)) :-
  decodeSignature(Sig,Tp).
pickupDeclaration(ctpl(lbl("cns",3),
		       [strg(Nm),strg(FullNm),strg(Sig)]),
		  cnsDec(Nm,FullNm,CnTp)) :-
  decodeSignature(Sig,CnTp).

decodeConsMap(ctpl(_,Ctors),ConsMap) :-
  map(Ctors,import:pickupCtor,ConsMap).

pickupCtor(ctpl(_,[Lbl,intgr(Ix)]),(Lbl,Ix)).
	    
loadCode(Strm,[]) :-
  at_end_of_stream(Strm).
loadCode(Strm,[Term|M]) :-
  read(Strm,Term),
  loadCode(Strm,M).

packageName(pkg(Nm,_),N) :-
  atom_string(N,Nm).

