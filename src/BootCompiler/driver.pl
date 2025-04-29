:- module(driver,[main/1,openR/5]).

/* Logic and Object compiler driver */

:- use_module(meta).
:- use_module(resource).
:- use_module(lexer).
:- use_module(opg).
:- use_module(astdisp).
:- use_module(canon).
:- use_module(checker).
:- use_module(display).
:- use_module(transform).
:- use_module(errors).
:- use_module(gensig).
:- use_module(gencode).
:- use_module(uri).
:- use_module(catalog).
:- use_module(macros).
:- use_module(misc).
:- use_module(import).
:- use_module(repository).
:- use_module(grapher).
:- use_module(lterms).

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
   map(G,driver:wrpPk,Gs),
   reportMsg("circular dependency in packages %s",[Gs]),!,
   fail).

wrpPk((P,_),pk(P)).

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
  (is_member(showAst,Opts) -> dispAst(Term) ; true),
  noErrors,
  macroPkg(Term,Prog),
  (is_member(showMacro,Opts) -> dispAst(Prog) ; true),
  (\+ is_member(macroOnly,Opts), noErrors ->
   checkProgram(Prog,Pkg,Repo,Opts,PkgDecls,Canon),!,
   (is_member(showTCCode,Opts) -> dispCanonProg(Canon);true),
   noErrors,
   (\+ is_member(typeCheckOnly,Opts) ->
    transformProg(PkgDecls,Canon,Opts,Rules),
    validLProg(PkgDecls,Rules),!,
    (is_member(showTrCode,Opts) -> dispProg(Rules);true),
    noErrors,
    (\+ is_member(transformOnly,Opts) ->
     genCode(PkgDecls,Rules,Opts,Text),
     noErrors,
     genPkgSig(Rules,Sig),		% goes into the repo manifest
     addCodePackage(Repo,SrcUri,Pkg,Sig,Text,Rx);
     true);
    true);
   true).

processStdin(Pkg,Repo,Opts) :-
  startCount,
  readStdinput(Src),
  parseFile(Pkg,Src,Term),!,
  noErrors,
  macroPkg(Term,Prog),
  noErrors,
  checkProgram(Prog,Pkg,Repo,Opts,_,_),!.

parseFile(Pkg,Txt,Term) :-
  allTokens(Pkg,Txt,Toks),
  parse(Toks,Term,Rem), !,
  (Rem=[Tok|_] ->
   locOfToken(Tok,Lc),
   reportError("extra token(s): %s on input",[Tok],Lc);true).

getSrcUri(Fl,WD,FU,FUri) :-
  parseURI(Fl,FU),
  resolveURI(WD,FU,FUri).
