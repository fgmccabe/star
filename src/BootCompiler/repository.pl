:- module(repository,[openRepository/2,
          locatePackage/6,
          openPackageAsStream/6,
          openPrologPackageAsStream/6,
          addPrologPackage/6,
          prologPackagePresent/7,
          addGoLangPackage/6]).

% Implement a file-based repository.

:- use_module(uri).
:- use_module(misc).
:- use_module(resource).
:- use_module(parseUtils).
:- use_module(manifest).

openRepository(Root,repo(Root,Manifest)) :-
  resolveFile(Root,"manifest",MF),
  access_file(MF,read),
  readManifest(MF,Manifest),!.
openRepository(Root,repo(Root,man([]))) :-
  resolveFile(Root,"manifest",MF),
  access_file(MF,write),!.

locatePackage(repo(Root,Man),Pkg,Act,Sig,U,Text) :-
  locateVersion(Man,Pkg,Act,Sig,U,Fn),
  resolveFile(Root,Fn,FileNm),
  readFile(FileNm,Text).

resolveFile(Root,Fl,MF) :-
  parseURI(Fl,Rel),
  resolveURI(Root,Rel,F),
  getUriPath(F,MF).

openPackageAsStream(repo(Root,Man),Pkg,Act,Sig,U,Stream) :-
  locateVersion(Man,Pkg,Act,U,Sig,fl(Fn)),
  resolveFile(Root,Fn,Fl),
  open(Fl,read,Stream).

openPrologPackageAsStream(repo(Root,Man),Pkg,Act,Sig,U,Stream) :-
  locateVersion(Man,Pkg,Act,U,Sig,fl(PrFn)),
  resolveFile(Root,PrFn,Fl),
  open(Fl,read,Stream).

locateVersion(man(Entries),pkg(Pkg,Vers),Act,Sig,U,Fn) :-
  is_member(entry(Pkg,Versions),Entries),
  getVersion(Vers,Versions,Act,Sig,U,Fn).

getVersion(Vers,V,pkg(Pkg,Vers),Sig,U,Fn) :- is_member((pkg(Pkg,Vers),Sig,U,Fn),V),!.
getVersion(defltVersion,V,Act,Sig,U,Fn) :- is_member((Act,Sig,U,Fn),V),!.

addPrologPackage(repo(Root,Man),U,pkg(Pkg,Vers),Sig,Text,repo(Root,NM)) :-
  packageHash(Pkg,Vers,Hash),
  string_concat(Pkg,Hash,Fn),
  string_concat(Fn,".pl",PrFn),
  resolveFile(Root,PrFn,FileNm),
  writeFile(FileNm,Text),!,
  addToManifest(Man,U,Pkg,Vers,Sig,fl(PrFn),NM),
  flushManifest(Root,NM).


addGoLangPackage(repo(Root,Man),U,pkg(Pkg,Vers),Sig,Text,repo(Root,NM)) :-
  packageHash(Pkg,Vers,Hash),
  string_concat(Pkg,Hash,Fn),
  string_concat(Fn,".go",GoFn),
  resolveFile(Root,GoFn,FileNm),
  writeFile(FileNm,Text),!,
  addToManifest(Man,U,Pkg,Vers,Sig,fl(PrFn),NM),
  flushManifest(Root,NM).

packageHash(Pkg,defltVersion,Hash) :-
  stringHash(0,Pkg,H),
  hashSixtyFour(H,Hash).
packageHash(Pkg,ver(V),Hash) :-
  stringHash(0,Pkg,H1),
  stringHash(H1,V,H2),
  hashSixtyFour(H2,Hash).

prologPackagePresent(repo(Root,Man),Pkg,Act,Sig,U,SrcWhen,When) :-
  locateVersion(Man,Pkg,Act,U,Sig,fl(PrFn)),
  resolveFile(Root,PrFn,FileNm),
  access_file(FileNm,read),
  time_file(FileNm,When),
  getUriPath(U,SrcFn),
  time_file(SrcFn,SrcWhen).

flushManifest(Root,M) :-
  resolveFile(Root,"manifest",Fn),
  writeManifest(Fn,M).

%% Each end-point directory has a manifest file in it.
% The role of the manifest is to map URIs to files

addToManifest(man(M),U,Pkg,Version,Sig,FileName,man(NM)) :-
  addEntry(M,U,Pkg,Version,Sig,FileName,NM).

addEntry([],U,Pkg,Version,Sig,FileName,[(entry(Pkg,[(pkg(Pkg,Version),Sig,U,FileName)]))]).
addEntry([entry(Pkg,Vers)|E],U,Pkg,Version,Sig,FileName,[entry(Pkg,NV)|E]) :- !,
  addVersion(Vers,U,pkg(Pkg,Version),Sig,FileName,NV).
addEntry([E|M],U,Pkg,Version,Sig,FileName,[E|R]) :-
  addEntry(M,U,Pkg,Version,Sig,FileName,R).

addVersion([],U,Vers,Sig,FileNm,[(Vers,U,Sig,FileNm)]).
addVersion([(Vers,_,_,_)|V],U,Vers,Sig,FileNm,[(Vers,U,Sig,FileNm)|V]) :- !. % replace version
addVersion([V|M],U,Vers,Sig,FileNm,[V|R]) :- addVersion(M,U,Vers,Sig,FileNm,R).
