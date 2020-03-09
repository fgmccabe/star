:- module(driver,[main/1,test/1,openR/5]).

/* Logic and Object compiler driver */

:- use_module(resource).
:- use_module(lexer).
:- use_module(grammar).
:- use_module(display).
:- use_module(canon).
:- use_module(checker).
:- use_module(transform).
:- use_module(errors).
:- use_module(gensig).
:- use_module(gencode).
:- use_module(uri).
:- use_module(catalog).
:- use_module(misc).
:- use_module(import).
:- use_module(repository).
:- use_module(grapher).
:- use_module(lterms).

parseFlags([],CWD,CWD,[],[]).
parseFlags(['-g'|More],CWD,Cx,[debugging|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-p'|More],CWD,Cx,[profiling|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-i',W|M],CW,Cx,[installDir(CWD)|Opts],Files) :-!,
  atom_string(W,WN),
  parseURI(WN,WU),
  resolveURI(CW,WU,CWD),
  parseFlags(M,CW,Cx,Opts,Files).
parseFlags(['-w',W|M],CW,Cx,Opts,Files) :-!,
  atom_string(W,WN),
  parseURI(WN,WU),
  resolveURI(CW,WU,CWD),
  parseFlags(M,CWD,Cx,Opts,Files).
parseFlags(['-r', R|More],CWD,Cx,[repository(Repo)|Opts],Files) :-!,
  atom_string(R,RN),
  parseURI(RN,RU),
  resolveURI(CWD,RU,Ruri),
  openRepository(Ruri,Repo),
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-c'|More],CWD,Cx,[compileOnly|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-v', V|More],CWD,Cx,[ver(Vers)|Opts],Files) :-!,
  atom_string(V,Vers),
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-f'|More],CWD,Cx,[forceCompile|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-di'|More],CWD,Cx,[showGenCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-da'|More],CWD,Cx,[showAst|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-dt'|More],CWD,Cx,[showTCCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-dT'|More],CWD,Cx,[showTrCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['-dA'|More],CWD,Cx,[showSetCode|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['--stdin'|More],CWD,Cx,[processStdin|Opts],Files) :-!,
  parseFlags(More,CWD,Cx,Opts,Files).
parseFlags(['--'|More], CWD,CWD, [], Files) :- !, stringify(More,Files).
parseFlags(More, CWD,CWD, [], Files) :- stringify(More,Files).

stringify([],[]).
stringify([Name|More],[Fn|Files]) :-
  atom_string(Name,Fn),
  stringify(More,Files).

main(Args) :-
  startCount,
  getCWDUri(CW),
  parseFlags(Args,CW,CWD,Opts,Pkgs),!,
  openRepo(Opts,Repo),
%  showRepoManifest(Repo),
  locateCatalog(CWD,Cat),!,
  (is_member(processStdin,Opts) ->
   Pkgs = [Pkg|_],
   processStdin(pkg(Pkg,defltVersion),Repo,Opts);
   makeGraph(Repo,Cat,CWD,Pkgs,Groups),!,
   (processGroups(Groups,[],Repo,CWD,Opts),! ; reportMsg("aborting compiling",[]),!,fail)),!.

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
  searchForRepo(CWD,RepoUri),
  openRepository(RepoUri,Repo).

processGroups([],_,_,_,_).
processGroups([G|L],CPkgs,Repo,CWD,Opts) :-
  (length(G,1) ->
    processGroup(G,CPkgs,CP0,Repo,R0,CWD,Opts),!,
    processGroups(L,CP0,R0,CWD,Opts) ;
    reportError("circular dependency in packages %s",G),
    fail).

processGroup([],CP,CP,Repo,Repo,_,_).
processGroup([(P,Imps,Fl)|L],CP,CPx,Repo,Rx,CWD,Opts) :-
  processPkg(P,Imps,Fl,CP,CP0,Repo,R0,Opts),!,
  processGroup(L,CP0,CPx,R0,Rx,CWD,Opts).

processPkg(P,Imps,_,CP,CP,Repo,Repo,Opts) :-
  importsOk(Imps,CP),
  pkgOk(P,Repo),
  \+ is_member(forceCompile,Opts),!.
processPkg(P,_,Fl,CP,[P|CP],Repo,Rx,Opts) :-
  reportMsg("compiling package %s",[P]),
  processFile(Fl,P,Repo,Rx,Opts),!.

importsOk([],_).
importsOk([P|I],CP) :-
  \+ (is_member(AP,CP), consistentPkg(P,AP)),
  importsOk(I,CP).

processFile(SrcUri,Pkg,Repo,Rx,Opts) :-
  startCount,
  locateResource(SrcUri,Src),
  parseFile(Pkg,Src,Term),!,
  noErrors,
  (is_member(showAst,Opts) -> display(Term) ; true),
  checkProgram(Term,Pkg,Repo,Opts,Prog),!,
  (is_member(showTCCode,Opts) -> dispProg(Prog);true),
  noErrors,
  (\+ is_member(compileOnly,Opts) ->
   transformProg(Prog,Opts,Rules),!,
   (is_member(showTrCode,Opts) -> displayRules(Rules);true),
   noErrors,
   genPkgSig(Rules,Sig),
   genCode(Rules,Opts,Text),
   noErrors,
   addCodePackage(Repo,SrcUri,Pkg,Sig,Text,Rx);
   true).

processStdin(Pkg,Repo,Opts) :-
  startCount,
  readStdinput(Src),
  parseFile(Pkg,Src,Term),!,
  noErrors,
  checkProgram(Term,Pkg,Repo,Opts,_Prog),!.

packageVersion(Opts,ver(Vers)) :-
  is_member(ver(Vers),Opts),!.
packageVersion(_,defltVersion).

parseFile(Pkg,Txt,Term) :-
  allTokens(Pkg,Txt,Toks),
  parse(Toks,Term,_), !.

test(Fl) :-
  getCWDUri(CWD),
  processFile(Fl,CWD,[/*debugging*/]).

getSrcUri(Fl,WD,FU,FUri) :-
  parseURI(Fl,FU),
  resolveURI(WD,FU,FUri).
