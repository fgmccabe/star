:- module(manifest,[readManifest/2,jsonManifest/2,showManifest/3,manifestJson/2,writeManifest/2]).

:- use_module(parseUtils).
:- use_module(uri).
:- use_module(misc).
:- use_module(json).
:- use_module(resource).

readManifest(Path,Manifest) :-
  readFile(Path,Chars),
  phrase(parseJson(J),Chars),
  jsonManifest(J,Manifest).

jsonManifest(jColl(L),man(M)) :-
  jsonEntries(L,M).

jsonEntries([],[]).
jsonEntries([J|L],[Entry|More]) :-
  jsonEntry(J,Entry),
  jsonEntries(L,More).

jsonEntry((P,jColl(V)),entry(P,Versions)) :-
  jsonVersions(P,V,Versions).

jsonVersions(_,[],[]).
jsonVersions(P,[V|L],[Vers|More]) :-
  jsonVersion(P,V,Vers),
  jsonVersions(P,L,More).

jsonVersion(P,("*",jColl(Dtl)),(pkg(P,defltVersion),Sig,SrcUri,fl(Code))) :-!,
  is_member(("source",jTxt(Src)),Dtl),
  is_member(("code",jTxt(Code)),Dtl),
  is_member(("signature",jTxt(Sig)),Dtl),
  parseURI(Src,SrcUri).
jsonVersion(P,(V,jColl(Dtl)),(pkg(P,ver(V)),Sig,SrcUri,fl(Code))) :-
  is_member(("source",jTxt(Src)),Dtl),
  is_member(("code",jTxt(Code)),Dtl),
  is_member(("signature",jTxt(Sig)),Dtl),
  parseURI(Src,SrcUri).

showManifest(man(E),O,Ox) :-
  appStr("manifest",O,O1),
  appStr("{\n",O1,O2),
  showEntries(E,O2,O3),
  appStr("}\n",O3,Ox).

showEntries([],O,O).
showEntries([E|M],O,Ox) :-
  showEntry(E,O,O1),
  showEntries(M,O1,Ox).

showEntry(entry(Pkg,Versions),O,Ox) :-
  appStr("  ",O,O0),
  appStr(Pkg,O0,O1),
  appStr(":{\n",O1,O2),
  showVersions(Versions,O2,O3),
  appStr("  }\n",O3,Ox).

showVersions([],O,O).
showVersions([V|M],O,Ox) :-
  showVersion(V,O,O1),
  showVersions(M,O1,Ox).

showVersion((pkg(_,V),Sig,U,F),O,Ox) :-
  appStr("    ",O,O1),
  showV(V,O1,O2),
  appStr(":",O2,O3),
  appQuoted(Sig,'"',O3,O4),
  appStr("@",O4,O5),
  showFileName(F,O5,O6),
  appStr("[",O6,O7),
  showUri(U,O7,O8),
  appStr("]\n",O8,Ox).

showV(ver(V),O,Ox) :-
  appStr(V,O,Ox).
showV(defltVersion,O,Ox) :-
  appStr("*",O,Ox).

showFileName(fl(Nm),O,Ox) :-
  appStr(Nm,O,Ox).

manifestJson(man(M),jColl(C)) :-
  manifestEntries(M,C).

manifestEntries([],[]).
manifestEntries([entry(Pkg,Versions)|L],[(Pkg,jColl(Entry))|M]) :-
  manifestVersions(Versions,Entry),
  manifestEntries(L,M).

manifestVersions([],[]).
manifestVersions([V|L],[J|M]) :-
  manifestVersion(V,J),
  manifestVersions(L,M).

manifestVersion((pkg(_,defltVersion),Uri,Sig,fl(CodeFn)), ("*",VV)) :-
  manifestDetails(CodeFn,Uri,Sig,VV).
manifestVersion((pkg(_,ver(Ver)),Uri,Sig,fl(CodeFn)), (Ver,VV)) :-
  manifestDetails(CodeFn,Uri,Sig,VV).

manifestDetails(Fn,Uri,Sig,jColl([("source",jTxt(U)),("code",jTxt(Fn)),("signature",jTxt(Sig))])) :-
  showUri(Uri,C,[]),
  string_chars(U,C).

writeManifest(Fn,M) :-
  manifestJson(M,Json),
  dispJson(Json,Chars,[]),
  string_chars(Text,Chars),
  writeFile(Fn,Text).
