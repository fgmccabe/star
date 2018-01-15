:- module(gensig,[genPkgSig/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genPkgSig(module(Pkg,Imports,Fields,Types,Enums,Defs,Contracts,Impls),Sig) :-
  constructPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Term),
  encodeTerm(Term,Chrs,[]),
  string_chars(Sig,Chrs).

constructPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,
    tpl([PkgNm,tpl(Imps),strg(Sig),tpl(ClsSigs),tpl(ConSigs),tpl(ImplSigs)])) :-
  encPkg(Pkg,PkgNm),
  encImports(Imports,Imps),
  encType(faceType(Fields,Types),Sig),
  formatEnums(Enums,ClsSigs),
  formatContracts(Contracts,ConSigs),
  formatImpls(Impls,ImplSigs).

encPkg(pkg(Nm,Vers),cons(strct("pkg",2),[strg(Nm),V])) :-
  encVer(Vers,V).

encVer(defltVersion,enum("*")).
encVer(ver(V),strg(V)).

encImports([],[]).
encImports([I|M],[IP|L]) :-
  encImport(I,IP),
  encImports(M,L).

encImport(import(Viz,Pkg,_,_,_,_,_),cons(strct("import",2),[enum(Viz),Enc])) :-
  encPkg(Pkg,Enc).

formatEnums([],[]).
formatEnums([Nm|M],[strg(Nm)|R]) :-
  formatEnums(M,R).

formatContracts([],[]).
formatContracts([conDef(Nm,CnNm,Spec)|M],[tpl([strg(Nm),strg(CnNm),strg(CSig)])|R]) :-
  encodeType(Spec,CChars,[]),
  string_chars(CSig,CChars),
  formatContracts(M,R).

formatImpls([],[]).
formatImpls([imp(Nm,Spec)|M],[tpl([strg(Nm),strg(Sig)])|R]) :-
  encodeConstraint(Spec,Chars,[]),
  string_chars(Sig,Chars),
  formatImpls(M,R).
