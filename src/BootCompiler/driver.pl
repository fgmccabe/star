:- module(driver,[main/1,test/1,openR/5]).

/* Logic and Object compiler driver */

:- use_module(resource).
:- use_module(lexer).
:- use_module(grammar).
:- use_module(display).
:- use_module(canon).
:- use_module(checker).
:- use_module(transform).
:- use_module(plog).
:- use_module(errors).
:- use_module(genprolog).
:- use_module(uri).
:- use_module(catalog).
:- use_module(misc).
:- use_module(import).
:- use_module(repository).
:- use_module(grapher).

parseFlags([],CWD,CWD,[],[]).
parseFlags(['-g'|More],CWD,Cx,[debugging|Opts],Files) :-
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-p'|More],CWD,Cx,[profiling|Opts],Files) :-
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-w',W|M],CW,Cx,Opts,Files) :-
  atom_string(W,WN),
  parseURI(WN,WU),
  resolveURI(CW,WU,CWD),
  parseFlags(M,CWD,Cx,Opts,Files).
parseFlags(['-r', R|More],CWD,Cx,[repository(Repo)|Opts],Files) :-
  atom_string(R,RN),
  parseURI(RN,RU),
  resolveURI(CWD,RU,Ruri),
  openRepository(Ruri,Repo),
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-v', V|More],CWD,Cx,[version(Vers)|Opts],Files) :-
  atom_string(V,Vers),
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['--'|More], CWD,CWD, [], Files) :- stringify(More,Files).
parseFlags(More, CWD,CWD, [], Files) :- stringify(More,Files).

stringify([],[]).
stringify([Name|More],[Fn|Files]) :-
  atom_string(Name,Fn),
  stringify(More,Files).

main(Args) :-
  startCount,
  getCWDUri(CW),
  parseFlags(Args,CW,CWD,Opts,Pkgs),
  openRepo(Opts,Repo),
  locateCatalog(CWD,Cat),!,
  makeGraph(Repo,Cat,CWD,Pkgs,Groups),!,
  (processGroups(Groups,[],Repo,CWD,Opts) ; reportMsg("aborting compiling",[])),!.

openR(Args,CWD,Cat,Repo,Groups) :-
  getCWDUri(CW),
  parseFlags(Args,CW,CWD,Opts,Pkgs),
  openRepo(Opts,Repo),
  locateCatalog(CWD,Cat),
  makeGraph(Repo,Cat,CWD,Pkgs,Groups).

openRepo(Opts,Repo) :-
  is_member(repository(Repo),Opts),!.
openRepo(_,Repo) :-
  getCWDUri(CWD),
  openRepository(CWD,Repo).

processGroups([],_,_,_,_).
processGroups([G|L],CPkgs,Repo,CWD,Opts) :-
  (length(G,1) -> true ; reportError("circular dependency in packages %s",G)),
  processGroup(G,CPkgs,CP0,Repo,R0,CWD,Opts),!,
  processGroups(L,CP0,R0,CWD,Opts).

processGroup([],CP,CP,Repo,Repo,_,_).
processGroup([(P,Imps,Fl)|L],CP,CPx,Repo,Rx,CWD,Opts) :-
  processPkg(P,Imps,Fl,CP,CP0,Repo,R0,Opts),!,
  processGroup(L,CP0,CPx,R0,Rx,CWD,Opts).

processPkg(P,Imps,_,CP,CP,Repo,Repo,_) :-
  importsOk(Imps,CP),
  pkgOk(P,Repo),!,
  reportMsg("skipping package %s",[P]).
processPkg(P,_,Fl,CP,[P|CP],Repo,Rx,Opts) :-
  reportMsg("compiling package %s",[P]),
  processFile(Fl,P,Repo,Rx,Opts),!.

importsOk([],_).
importsOk([P|I],CP) :-
  \+ (is_member(AP,CP), consistentPkg(P,AP)),
  importsOk(I,CP).

processFile(SrcUri,Pkg,Repo,Rx,Opts) :-
  packageVersion(Opts,Vers),
  startCount,
  locateResource(SrcUri,Src),
  parseFile(Src,Term),!,
  noErrors,
  checkProgram(Term,Vers,Repo,Prog),!,
  dispProg(Prog),
  noErrors,
  transformProg(Prog,Opts,Rules),!,
  noErrors,
  genRules(Rules,Text),
  addPrologPackage(Repo,SrcUri,Pkg,Text,Rx).

packageVersion(Opts,v(Vers)) :-
  is_member(version(Vers),Opts),!.
packageVersion(_,defltVersion).

parseFile(Txt,Term) :-
  allTokens(Txt,Toks),
  parse(Toks,Term,_), !.

test(Fl) :-
  getCWDUri(CWD),
  processFile(Fl,CWD,[/*debugging*/]).

getCWDUri(WD) :-
  working_directory(C,C),
  atom_string(C,D),
  string_concat("file:",D,DT),
  parseURI(DT,WD).

getSrcUri(Fl,WD,FU,FUri) :-
  parseURI(Fl,FU),
  resolveURI(WD,FU,FUri).
