:- module(gencode,[compCode/3]).

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

compCode(module(Pkg,Imports,Fields,Types,Enums,Defs,Contracts,Impls),Sig,Text) :-
  genPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Sig),
  genPlDefs(Defs,Chrs,[]),
  string_chars(Text,Chrs).

genPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Sig) :-
  constructPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Term),
  encodeTerm(Term,Chrs,[]),
  string_chars(Sig,Chrs).

constructPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,
    tpl([PkgNm,tpl(Imps),FTps,tpl(ClsSigs),tpl(ConSigs),tpl(ImplSigs)])) :-
  encPkg(Pkg,PkgNm),
  encImports(Imports,Imps),
  encType(faceType(Fields,Types),FTps),
  formatEnums(Enums,ClsSigs),
  formatContracts(Contracts,ConSigs),
  formatImpls(Impls,ImplSigs).

encType(Tp,strg(Sig)) :-
  encodeType(Tp,O,[]),
  string_chars(Sig,O).

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
