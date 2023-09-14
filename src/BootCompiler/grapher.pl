:- module(grapher,[scanPkg/6,makeGraph/5,pkgOk/2,consistentPkg/2]).

:- use_module(location).
:- use_module(topsort).
:- use_module(uri).
:- use_module(resource).
:- use_module(lexer).
:- use_module(opg).
:- use_module(repository).
:- use_module(manifest).
:- use_module(misc).
:- use_module(abstract).
:- use_module(import).
:- use_module(errors).
:- use_module(catalog).

makeGraph(Repo,Cat,CWD,Fls,Groups) :-
  scanPkgs(Fls,Repo,Cat,CWD,[],Pkgs),
  graphPkgs(Pkgs,Groups).

graphPkgs(Pkgs,Groups) :-
  topsort(Pkgs,Groups,grapher:consistentPkg).
  %showGroups(Groups).

scanPkgs([],_,_,_,Pkgs,Pkgs).
scanPkgs([P|L],Repo,Cat,CWD,SoFar,Pkgs) :-
  parsePkgName(P,Cat,Pkg),
  scanPkg(Pkg,Repo,Cat,CWD,SoFar,P1),
  scanPkgs(L,Repo,Cat,CWD,P1,Pkgs).

scanPkg(pkg(Pkg,RqV),_,_,_,SoFar,SoFar) :-
  is_member((pkg(Pkg,V),_,_,_),SoFar),
  consistentVersion(RqV,V),!.
scanPkg(Pkg,Repo,Cat,CWD,SoFar,Pkgs) :-
  codePackagePresent(Repo,Pkg,_,_,SrcFn,SrcWhen,CodeWhen),
  ( CodeWhen>SrcWhen ->
    importPkg(Pkg,Repo,Spec),
    checkPkg(Spec,Repo,Cat,CWD,SrcFn,SoFar,Pkgs) ;
    scanCat(Cat,Repo,Pkg,CWD,SoFar,Pkgs)).
scanPkg(Pkg,Repo,Cat,CWD,Pi,Px) :-
    scanCat(Cat,Repo,Pkg,CWD,Pi,Px).

parsePkgName(P,_,pkg(Pkg,Version)) :-
  sub_string(P,Before,_,After,":"),!,
  sub_string(P,0,Before,_,Pkg),
  sub_string(P,_,After,0,Version).
parsePkgName(P,Cat,Pkg) :-
  resolveVersion(pkg(P,defltVersion),Cat,Pkg).

checkPkg(spec(Pkg,Imports,_),Repo,Cat,CWD,SrcFn,SoFar,Pkgs) :-
  reformatImports(Imports,Imps,Cat),
  scanImports(Imps,Repo,Cat,CWD,[(Pkg,Imps,Imps,SrcFn)|SoFar],Pkgs).

reformatImports([],[],_).
reformatImports([import(_,P)|L],[Pkg|M],Cat) :-
  resolveVersion(P,Cat,Pkg),
  reformatImports(L,M,Cat).

scanImports([],_,_,_,Pkgs,Pkgs).
scanImports([Pkg|Imports],Repo,Cat,CWD,SoFar,Pkgs) :-
  scanPkg(Pkg,Repo,Cat,CWD,SoFar,Pkg1),
  scanImports(Imports,Repo,Cat,CWD,Pkg1,Pkgs).

scanCat(Cat,Repo,Pkg,CWD,Pi,Px) :-
  ( resolveCatalog(Cat,Pkg,Uri,VPkg) -> scanFile(Uri,VPkg,Repo,Cat,CWD,Pi,Px) ;
    pkgLoc(Pkg,PLc),reportError("cannot locate package %s",[Pkg],PLc),Pi=Px).

scanFile(Fl,Pkg,Repo,Cat,CWD,SoFar,Pkgs) :-
  parseFile(Pkg,Fl,Term),
  scanForImports(Term,Imps,Cat),!,
  scanImports(Imps,Repo,Cat,CWD,[(Pkg,Imps,Imps,Fl)|SoFar],Pkgs).

parseFile(Pk,Fl,Term) :-
  locateResource(Fl,Txt),
  allTokens(Pk,Txt,Toks),
  parse(Toks,Term,_), !.

getSrcUri(Fl,WD,FUri) :-
  parseURI(Fl,FU),
  resolveURI(WD,FU,FUri).

scanForImports(Term,Imports,Cat) :-
  isBraceTerm(Term,_,_,Els),
  scanThetaEnv(Els,Imports,Cat),!.

scanPackageName(Term,pkg(Nm,Ver)) :-
  isBinary(Term,_,":",L,R),!,
  scanNm(L,Nm),
  scanNm(R,Ver).
scanPackageName(Term,pkg(Nm,defltVersion)) :-
  scanNm(Term,Nm).

scanNm(Term,Nm) :- isIden(Term,Nm).
scanNm(Term,Pk) :- isBinary(Term,_,".",L,R),
  scanNm(L,F),
  scanNm(R,B),
  string_concat(F,".",FF),
  string_concat(FF,B,Pk).
scanNm(Name,"") :-
  locOfAst(Name,Lc),
  reportError("Package name %s not valid",[Name],Lc).

scanThetaEnv([],[],_).
scanThetaEnv([St|Stmts],Imports,Cat) :-
    scanStmt(St,Imports,MoreImp,Cat),
    scanThetaEnv(Stmts,MoreImp,Cat).

scanStmt(St,Imp,More,Cat) :-
  isUnary(St,_,"public",El),!,
  scanStmt(El,Imp,More,Cat).
scanStmt(St,Imp,More,Cat) :-
  isUnary(St,_,"private",El),!,
  scanStmt(El,Imp,More,Cat).
scanStmt(St,[Pkg|More],More,Cat) :-
  isUnary(St,_,"import",P),
  scanPackageName(P,Pk),
  resolveVersion(Pk,Cat,Pkg).
scanStmt(_,Imp,Imp,_).

pkgOk(Pkg,Repo) :-
  codePackagePresent(Repo,Pkg,_,_,_U,SrcWhen,CodeWhen),
  CodeWhen>SrcWhen.

showGroups([]).
showGroups([G|L]) :-
  writef("---\n"),
  showGroup(G),
  showGroups(L).

showGroup([]).
showGroup([P|M]) :-
  showPkgDeps(P),
  showGroup(M).

showPkgDeps((Pkg,Imps,_)) :-
  writef("Package:"),
  showPkg(Pkg),
  write("-->"),
  showPkgs(Imps,""),
  write("\n").

showPkgs([],_).
showPkgs([P|L],S) :-
  write(S),
  showPkg(P),
  showPkgs(L," ").

showPkg(pkg(P,defltVersion)) :-
  writef("%w",[P]).
showPkg(pkg(P,ver(V))) :-
  writef("%w:%w",[P,V]).
