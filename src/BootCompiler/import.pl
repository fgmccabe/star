:- module(import, [importPkg/3,loadPkg/4,loadPrologPkg/4,consultPrologPkg/3]).

% This is specific to the Prolog translation of L&O code

:- use_module(resource).
:- use_module(types).
:- use_module(misc).
:- use_module(uri).
:- use_module(transutils).
:- use_module(decode).
:- use_module(repository).

importPkg(Pkg,Repo,spec(Act,Export,Types,Classes,Contracts,Implementations,Imports)) :-
  openPrologPackageAsStream(Repo,Pkg,Act,_,Strm),
  read(Strm,SigTerm),
  close(Strm),
  pickupPkgSpec(SigTerm,Pkg,Imports,Export,Types,Classes,Contracts,Implementations).

pickupPkgSpec('#pkg'(Enc),Pkg,Imports,Export,Types,Classes,Contracts,Implementations) :-
  decodeValue(Enc,tpl([Pk,tpl(Imps),FTps,TTps,tpl(ClsSigs),tpl(ConSigs),tpl(ImplSigs)])),
  pickupPkg(Pk,Pkg),
  pickupImports(Imps,Imports),
  pickupFace(FTps,Export),
  pickupFace(TTps,Types),
  pickupClasses(ClsSigs,Classes,[]),
  pickupContracts(ConSigs,Contracts),
  pickupImplementations(ImplSigs,Implementations,[]).

pickupPkg(cons(strct("pkg",2),[strg(Nm),V]),pkg(Nm,Vers)) :-
  pickupVersion(V,VV),
  (consistentVersion(Vers,VV) ; writef("version of imported pkg %w#%w not consistent with %w",[Nm,VV,Vers])).

pickupVersion(enum("*"),defltVersion).
pickupVersion(strg(V),ver(V)).

consistentVersion(defltVersion,_).
consistentVersion(ver(V),ver(V)).

pickupImports([],[]).
pickupImports([cons(strct("import",2),[V,P])|L],[import(Viz,Pkg)|M]) :-
  pickupViz(V,Viz),
  pickupPkg(P,Pkg),
  pickupImports(L,M).

pickupViz(enum("private"),private).
pickupViz(enum("public"),public).

pickupFace(strg(Sig),Type) :-
  decodeSignature(Sig,Type).

pickupClasses([],Cls,Cls).
pickupClasses([strg(Nm)|Rest],[Nm|More],Cls):-
  pickupClasses(Rest,More,Cls).

pickupContracts(C,Cons) :-
  findContracts(C,Cons,[]).

findContracts([],C,C).
findContracts([tpl([strg(Nm),strg(CnNm),strg(Sig)])|M],[contract(Nm,CnNm,Spec)|C],Cx) :-
  decodeType(Sig,Spec),
  findContracts(M,C,Cx).

processImplementations(Env,Impls,MoreImpls) :-
  decodeValue(Env,tpl(Els)),
  pickupImplementations(Els,Impls,MoreImpls).

pickupImplementations([],I,I).
pickupImplementations([tpl([strg(Nm),strg(Sig)])|M],[imp(Nm,Spec)|I],RI) :-
  decodeConstraint(Sig,Spec),
  pickupImplementations(M,I,RI).

loadPrologPkg(Pkg,Repo,Code,Imports) :-
  openPrologPackageAsStream(Repo,Pkg,_,_,Strm),
  read(Strm,SigTerm),
  pickupPkgSpec(SigTerm,Pkg,Imports,_,_,_,_,_),
  loadCode(Strm,Code),
  close(Strm).

loadCode(Strm,[]) :-
  at_end_of_stream(Strm).
loadCode(Strm,[Term|M]) :-
  read(Strm,Term),
  loadCode(Strm,M).

consultPrologPkg(Pkg,Repo,Imports) :-
  openPrologPackageAsStream(Repo,Pkg,_,_,Strm),
  packageName(Pkg,Nm),
  read(Strm,SigTerm),
  pickupPkgSpec(SigTerm,Pkg,Imports,_,_,_,_,_),
  load_files(Nm,[stream(Strm)]),
  close(Strm).

packageName(pkg(Nm,_),N) :-
  atom_string(N,Nm).

loadPkg(Pkg,Repo,Code,Imports) :-
  openPackageAsStream(Repo,Pkg,_,_,Strm),
  read(Strm,SigTerm),
  pickupPkgSpec(SigTerm,Pkg,Imports,_,_,_,_,_),
  loadCode(Strm,Code),
  close(Strm).
