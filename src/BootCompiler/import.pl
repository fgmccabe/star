:- module(import, [importPkg/3,loadPkg/4,loadPrologPkg/4,consultPrologPkg/3]).

:- use_module(resource).
:- use_module(types).
:- use_module(misc).
:- use_module(uri).
:- use_module(transutils).
:- use_module(decode).
:- use_module(repository).

importPkg(Pkg,Repo,spec(Act,Face,Classes,Contracts,Implementations,Imports)) :-
  codePackagePresent(Repo,Pkg,Act,Sig,_U,_SrcWhen,_When),
  pickupPkgSpec(Sig,Pkg,Imports,Face,Classes,Contracts,Implementations).

pickupPkgSpec(Enc,Pkg,Imports,Face,Classes,Contracts,Implementations) :-
  decodeValue(Enc,ctpl(_,[Pk,ctpl(_,Imps),FTps,ctpl(_,ClsSigs),ctpl(_,ConSigs),ctpl(_,ImplSigs)])),
  pickupPkg(Pk,Pkg),
  pickupImports(Imps,Imports),
  pickupFace(FTps,Face),
  pickupClasses(ClsSigs,Classes,[]),
  pickupContracts(ConSigs,Contracts),
  pickupImplementations(ImplSigs,Implementations,[]).

pickupPkg(ctpl(lbl("pkg",2),[strg(Nm),V]),pkg(Nm,Vers)) :-
  pickupVersion(V,VV),
  (consistentVersion(Vers,VV) ; writef("version of imported pkg %w#%w not consistent with %w",[Nm,VV,Vers])).

pickupVersion(enum("*"),defltVersion).
pickupVersion(strg(V),ver(V)).

consistentVersion(defltVersion,_).
consistentVersion(ver(V),ver(V)).

pickupImports([],[]).
pickupImports([ctpl(lbl("import",2),[V,P])|L],[import(Viz,Pkg)|M]) :-
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
findContracts([ctpl(_,[strg(Nm),strg(CnNm),strg(Sig)])|M],[conDef(Nm,CnNm,Spec)|C],Cx) :-
  decodeSignature(Sig,Spec),
  findContracts(M,C,Cx).

processImplementations(Env,Impls,MoreImpls) :-
  decodeValue(Env,ctpl(_,Els)),
  pickupImplementations(Els,Impls,MoreImpls).

pickupImplementations([],I,I).
pickupImplementations([ctpl(_,[strg(Nm),strg(Sig)])|M],[imp(Nm,Spec)|I],RI) :-
  decodeSignature(Sig,Spec),
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
