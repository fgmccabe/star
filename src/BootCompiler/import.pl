:- module(import, [importAll/3,importPkg/4,loadPkg/4]).


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
  closure(Imports,[],import:notAlreadyImported,import:importMore(Repo),AllImports).

importMore(Repo,import(Lc,Viz,Pkg),SoFar,[import(Lc,Viz,Pkg)|SoFar],Inp,More) :-
  importPkg(Pkg,Lc,Repo,spec(_,_,_,_,Imports)),
  addPublicImports(Imports,Inp,More).
importMore(_,import(Lc,_,Pkg),SoFar,SoFar,Inp,Inp) :-
  reportError("could not import package %s",[Pkg],Lc).

notAlreadyImported(import(_,_,Pkg),SoFar) :-
  \+ is_member(import(_,_,Pkg),SoFar),!.

addPublicImports([],Imp,Imp).
addPublicImports([import(Lc,public,Pkg)|I],Rest,[import(Lc,transitive,Pkg)|Out]) :-
  addPublicImports(I,Rest,Out).
addPublicImports([import(_,private,_)|I],Rest,Out) :-
  addPublicImports(I,Rest,Out).
addPublicImports([import(_,transitive,_)|I],Rest,Out) :-
  addPublicImports(I,Rest,Out).

importPkg(Pkg,Lc,Repo,
	  spec(Act,Face,Decls,Imports)) :-
  codePackagePresent(Repo,Pkg,Act,Sig,_U,_SrcWhen,_When),
  pickupPkgSpec(Sig,Lc,Pkg,Imports,Face,Decls).

pickupPkgSpec(Enc,Lc,Pkg,Imports,Face,Decls) :-
  decodeValue(Enc,ctpl(_,[Pk,ctpl(_,Imps),Tp,ctpl(_,DeclSigs)])),
  pickupPkg(Pk,Pkg),
  pickupImports(Imps,Lc,Imports),
  pickupType(Tp,Face),
  pickupDeclarations(DeclSigs,Decls).

pickupPkg(ctpl(lbl("pkg",2),[strg(Nm),V]),pkg(Nm,Vers)) :-
  pickupVersion(V,VV),
  (consistentVersion(Vers,VV) ; writef("version of imported pkg %w#%w not consistent with %w",[Nm,VV,Vers])).

pickupVersion(enum("*"),defltVersion).
pickupVersion(strg(V),ver(V)).

consistentVersion(defltVersion,_).
consistentVersion(ver(V),ver(V)).

pickupImports([],_,[]).
pickupImports([ctpl(lbl("import",2),[V,P])|L],Lc,[import(Lc,Viz,Pkg)|M]) :-
  pickupViz(V,Viz),
  pickupPkg(P,Pkg),
  pickupImports(L,Lc,M).

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
pickupDeclaration(ctpl(lbl("acc",3),
		       [strg(Sig),strg(Fld),strg(Fn),strg(AccSig)]),
		  accessDec(Tp,Fld,Fn,AccTp)) :-
  decodeSignature(Sig,Tp),
  decodeSignature(AccSig,AccTp).
pickupDeclaration(ctpl(lbl("con",4),
		       [strg(Nm),strg(CnNm),strg(TSig),strg(Sig)]),
		  contractDec(Nm,CnNm,CnTp,Spec)) :-
  decodeSignature(Sig,Spec),
  decodeSignature(TSig,CnTp).
pickupDeclaration(ctpl(lbl("tpe",3),[strg(Sig),strg(RlSig),ctpl(_,ConsEnc)]),
		  typeDec(Tp,TpRule,ConsMap)) :-
  decodeSignature(Sig,Tp),
  decodeSignature(RlSig,TpRule),
  decodeConsMap(ConsEnc,ConsMap).
pickupDeclaration(ctpl(lbl("var",2),[strg(Nm),strg(Sig)]),varDec(Nm,Tp)) :-
  decodeSignature(Sig,Tp).
pickupDeclaration(ctpl(lbl("fun",2),[strg(Nm),strg(Sig)]),funDec(Nm,Tp)) :-
  decodeSignature(Sig,Tp).

decodeConsMap(ctpl(_,Ctors),ConsMap) :-
  map(Ctors,import:pickupCtor,ConsMap).

pickupCtor(ctpl(_,[strg(Nm),strg(FlNm),intgr(Ix),strg(Sig)]),(Nm,FlNm,Ix,Tp)) :-
  decodeSignature(Sig,Tp).

	    
loadCode(Strm,[]) :-
  at_end_of_stream(Strm).
loadCode(Strm,[Term|M]) :-
  read(Strm,Term),
  loadCode(Strm,M).

packageName(pkg(Nm,_),N) :-
  atom_string(N,Nm).

loadPkg(Pkg,Repo,Code,Imports) :-
  openPackageAsStream(Repo,Pkg,_,_,Strm),
  read(Strm,SigTerm),
  pickupPkgSpec(SigTerm,Pkg,Imports,_,_,_,_,_,_),
  loadCode(Strm,Code),
  close(Strm).
