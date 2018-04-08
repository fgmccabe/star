:- module(gensig,[genPkgSig/2,encPkg/2]).

:- use_module(misc).
:- use_module(terms).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genPkgSig(mdule(Pkg,Imports,Face,Enums,_Defs,Contracts,Impls),Sig) :-
  constructPkgSig(Pkg,Imports,Face,Enums,Contracts,Impls,Term),
  encodeTerm(Term,Chrs,[]),
  string_chars(Sig,Chrs).

constructPkgSig(Pkg,Imports,Face,Enums,Contracts,Impls,SigTpl) :-
  mkTpl([PkgNm,ImpTpl,strg(Sig),ClsTpl,ConTpl,ImplTpl],SigTpl),
  encPkg(Pkg,PkgNm),
  encImports(Imports,Imps),
  mkTpl(Imps,ImpTpl),
  encType(Face,Sig),
  formatEnums(Enums,ClsSigs),
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

encImport(import(Viz,Pkg,_,_,_,_),ctpl(lbl("import",2),[enum(Viz),Enc])) :-
  encPkg(Pkg,Enc).

formatEnums([],[]).
formatEnums([Nm|M],[strg(Nm)|R]) :-
  formatEnums(M,R).

formatContracts([],[]).
formatContracts([conDef(Nm,CnNm,Spec)|M],[ConTpl|R]) :-
  encodeType(Spec,CChars,[]),
  string_chars(CSig,CChars),
  mkTpl([strg(Nm),strg(CnNm),strg(CSig)],ConTpl),
  formatContracts(M,R).

formatImpls([],[]).
formatImpls([imp(Nm,Spec)|M],[ImplTpl|R]) :-
  encodeType(Spec,Chars,[]),
  string_chars(Sig,Chars),
  mkTpl([strg(Nm),strg(Sig)],ImplTpl),
  formatImpls(M,R).
