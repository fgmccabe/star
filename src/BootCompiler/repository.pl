:- module(repository,[openRepository/2,
		      locatePackage/6,
		      openPackageAsStream/6,
		      openCodePackageAsStream/6,
		      codePackagePresent/7,
		      addCodePackage/6,
		      searchForRepo/2,
		      showRepoManifest/1
		      ]).

% Implement a file-based repository.

:- use_module(uri).
:- use_module(misc).
:- use_module(resource).
:- use_module(parseUtils).
:- use_module(manifest).
:- use_module(errors).

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
  locateVersion(Man,Pkg,Act,Sig,U,fl(Fn)),
  resolveFile(Root,Fn,Fl),
  open(Fl,read,Stream).

openCodePackageAsStream(repo(Root,Man),Pkg,Act,Sig,U,Stream) :-
  locateVersion(Man,Pkg,Act,Sig,U,fl(PrFn)),
  resolveFile(Root,PrFn,Fl),
  open(Fl,read,Stream).

locateVersion(man(Entries),pkg(Pkg,Vers),pkg(Pkg,Act),Sig,U,Fn) :-
  is_member(entry(Pkg,Versions),Entries),
  getVersion(Vers,Versions,Act,Sig,U,Fn).

getVersion(defltVersion,V,Act,Sig,U,Fn) :- !, is_member((Act,Sig,U,Fn),V),!.
getVersion(Vers,V,Vers,Sig,U,Fn) :- is_member((Vers,Sig,U,Fn),V),!.

addCodePackage(repo(Root,Man),U,pkg(Pkg,Vers),Sig,Text,repo(Root,NM)) :-
  versionName(Vers,VNm),
  string_concat(".",VNm,H),
  string_concat(Pkg,H,Fn),
  string_concat(Fn,".cafe",GoFn),
  resolveFile(Root,GoFn,FileNm),
  writeFile(FileNm,Text),!,
  addToManifest(Man,U,Pkg,Vers,Sig,fl(GoFn),NM),
  flushManifest(Root,NM).

versionName(ver(Nm),Nm).
versionName(defltVersion,"*").

showRepoManifest(repo(Root,Man)) :-
  reportMsg("manifest of repo at %s",[Root]),
  dispManifest(Man).

codePackagePresent(repo(Root,Man),Pkg,Act,Sig,U,SrcWhen,When) :-
  locateVersion(Man,Pkg,Act,Sig,U,fl(PrFn)),
  resolveFile(Root,PrFn,FileNm),
  access_file(FileNm,read),
  time_file(FileNm,When),
  getUriPath(U,SrcFn),
  time_file(SrcFn,SrcWhen).

flushManifest(Root,M) :-
  resolveFile(Root,"manifest",Fn),
  writeManifest(Fn,M).

searchForRepo(Root,RepoUri) :-
  parseURI(".star-repo/",RU),
  resolveURI(Root,RU,RepoUri),
  getUriPath(RepoUri,Repo),
  access_file(Repo,read),!.
searchForRepo(Root,Repo) :-
  parseURI("../",PU),
  resolveURI(Root,PU,Parent),!,
  searchForRepo(Parent,Repo).

%% Each end-point directory has a manifest file in it.
% The role of the manifest is to map URIs to files

addToManifest(man(M),U,Pkg,Version,Sig,FileName,man(NM)) :-
  addEntry(M,U,Pkg,Version,Sig,FileName,NM).

addEntry([],U,Pkg,Version,Sig,FileName,[(entry(Pkg,[(Version,Sig,U,FileName)]))]).
addEntry([entry(Pkg,Vers)|E],U,Pkg,Version,Sig,FileName,[entry(Pkg,NV)|E]) :- !,
  addVersion(Vers,U,Version,Sig,FileName,NV).
addEntry([E|M],U,Pkg,Version,Sig,FileName,[E|R]) :-
  addEntry(M,U,Pkg,Version,Sig,FileName,R).

addVersion([],U,Vers,Sig,FileNm,[(Vers,Sig,U,FileNm)]).
addVersion([(Vers,_,_,_)|V],U,Vers,Sig,FileNm,[(Vers,Sig,U,FileNm)|V]) :- !. % replace version
addVersion([V|M],U,Vers,Sig,FileNm,[V|R]) :- addVersion(M,U,Vers,Sig,FileNm,R).
