:- module(catalog,[locateCatalog/2,resolveCatalog/4,catalogBase/2]).

:- use_module(resource).
:- use_module(misc).
:- use_module(uri).
:- use_module(parseUtils).
:- use_module(json).

locateCatalog(Uri,Cat) :-
  resolveURI(Uri,relUri(rel(["catalog"]),noQuery),CatURI),
  locateResource(CatURI,Chars),
  parseJsonCat(Chars,CatURI,Cat),!.

resolveCatalog(cat(Cat),Nm,Uri,V) :-
  is_member(entries(Map),Cat),
  is_member(base(Base),Cat),
  is_member(entry(Nm,U),Map),!,
  resolveURI(Base,U,Uri),
  resolveVersion(Nm,Cat,V).
resolveCatalog(cat(Cat),Nm,Uri,V) :-
  is_member(default(Deflt),Cat),!,
  is_member(base(Base),Cat),
  resolveURI(Base,Deflt,DefltUri),
  locateCatalog(DefltUri,DefltCat),
  resolveCatalog(DefltCat,Nm,Uri,V).

resolveVersion(pkg(Pkg,defltVersion),Cat,pkg(Pkg,V)) :-
  catalogVersion(Cat,V).
resolveVersion(Pkg,_,Pkg).

catalogVersion(Cat,ver(V)) :-
  is_member(ver(V),Cat),!.
catalogVersion(_,defltVersion).

parseJsonCat(Chrs,U,Cat):-
  phrase(parseJson(J),Chrs),
  jsonCatalog(J,C),
  defaultBase(C,U,Cat).

jsonCatalog(jColl(L),cat(S)) :-
  jsonStmts(L,S),!.

jsonStmts([],[]).
jsonStmts([S|L],[C|CL]) :-
  jsonStmt(S,C),
  jsonStmts(L,CL).

jsonStmt(("content",jColl(C)),entries(JC)) :-
  jsonEntries(C,JC).
jsonStmt(("base",jTxt(B)),base(U)) :-
  parseURI(B,U).
jsonStmt(("version",jTxt(V)),ver(V)).
jsonStmt(("default",jTxt(D)),default(U)) :-
  parseURI(D,U).

jsonEntries([],[]).
jsonEntries([(Pk,jTxt(U))|C],[entry(pkg(Pk,defltVersion),Url)|JC]) :-
  jsonEntries(C,JC),
  parseURI(U,Url).

defaultBase(cat(Stmts),Fl,cat(NStmts)) :-
  replace(Stmts,base(_),base(Fl),NStmts).

catalogBase(cat(Stmts),Base) :-
  is_member(base(Base),Stmts).
