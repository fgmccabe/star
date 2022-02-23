:- module(catalog,[locateCatalog/2,
		   resolveCatalog/4,
		   catalogBase/2,
		   catalogVersion/2]).

:- use_module(resource).
:- use_module(misc).
:- use_module(uri).
:- use_module(parseUtils).
:- use_module(json).
:- use_module(errors).

locateCatalog(Uri,Cat) :-
  resolveURI(Uri,relUri(rel(["catalog"]),noQuery),CatURI),
  readCatalog(CatURI,Cat).

readCatalog(CatURI,Cat) :-
  locateResource(CatURI,Chars),!,
  (parseJsonCat(Chars,CatURI,Cat) ->
     true ;
     writef("cannot parse catalog at %t\n",[CatURI]),
     abort).

resolveCatalog(cat(Cat),Nm,Uri,V) :-
  is_member(entries(Map),Cat),
  is_member(base(Base),Cat),
  is_member(entry(Nm,U),Map),!,
  resolveURI(Base,U,Uri),
  resolveVersion(Nm,Cat,V).
resolveCatalog(cat(Cat),Nm,Uri,V) :-
  is_member(subcatalogs(L),Cat),
  is_member(Sub,L),
  resolveCatalog(Sub,Nm,Uri,V),!.
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
  jsonCatalog(J,U,C),
  defaultBase(C,U,Cat).

jsonCatalog(jColl(L),U,cat(S)) :-
  jsonStmts(L,U,S),!.

jsonStmts([],_,[]).
jsonStmts([S|L],U,[C|CL]) :-
  jsonStmt(S,U,C),
  jsonStmts(L,U,CL).

jsonStmt(("content",jColl(C)),_,entries(JC)) :-
  jsonEntries(C,JC).
jsonStmt(("base",jTxt(B)),_,base(U)) :-
  parseURI(B,U).
jsonStmt(("version",jTxt(V)),_,ver(V)).
jsonStmt(("default",jTxt(T)),_,default(U)) :-
  parseURI(T,U).
jsonStmt(("subcatalogs",jSeq(L)),U,subcatalogs(LU)) :-
  map(L,catalog:parseSubCatalog(U),LU).

parseSubCatalog(U,jTxt(S),Sub) :-
  parseURI(S,SU),
  resolveURI(U,SU,CatURI),
  readCatalog(CatURI,Sub).

parseJURI(jTxt(U),PU) :-
  parseURI(U,PU).

jsonEntries([],[]).
jsonEntries([(Pk,jTxt(U))|C],[entry(pkg(Pk,defltVersion),Url)|JC]) :-
  jsonEntries(C,JC),
  parseURI(U,Url).

defaultBase(cat(Stmts),Fl,cat(NStmts)) :-
  replace(Stmts,base(_),base(Fl),NStmts).

catalogBase(cat(Stmts),Base) :-
  is_member(base(Base),Stmts).
