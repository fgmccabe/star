:- module(manifest,[readManifest/2,
		    dispManifest/1,showManifest/3,
		    writeManifest/2]).

:- use_module(uri).
:- use_module(misc).
:- use_module(resource).
:- use_module(encode).
:- use_module(decode).

readManifest(Path,Manifest) :-
  readFile(Path,Chars),
  phrase(decodeTerm(T),Chars),!,
  termManifest(T,Manifest).

termManifest(lst(Els),man(M)) :-
  map(Els,manifest:termEntry,M).

termEntry(ctpl(lbl("()2",2),[strg(P),lst(Es)]),entry(P,Vs)) :-
  map(Es,manifest:termVersion,Vs).

termVersion(ctpl(lbl("()2",2),[strg("*"),lst(Dtls)]),
	    (defltVersion,Sig,SrcUri,fl(Code))) :- !,
  findStrg("source",Dtls,Src),
  findStrg("code",Dtls,Code),
  findStrg("signature",Dtls,Sig),
  parseURI(Src,SrcUri).

termVersion(ctpl(lbl("()2",2),[strg(V),lst(Dtls)]),
	    (ver(V),Sig,SrcUri,fl(Code))) :- !,
  findStrg("source",Dtls,Src),
  findStrg("code",Dtls,Code),
  findStrg("signature",Dtls,Sig),
  parseURI(Src,SrcUri).

findStrg(Nm,[ctpl(lbl(Nm,1),[strg(Vl)])|_],Vl) :- !.
findStrg(Nm,[_|Es],Vl) :-
  findStrg(Nm,Es,Vl).

manifestTerm(man(M),lst(Els)) :-
  map(M,manifest:entryTerm,Els).

entryTerm(entry(P,Vs),ctpl(lbl("()2",2),[strg(P),lst(Es)])) :-
  map(Vs,manifest:versionTerm,Es).

versionTerm((defltVersion,Sig,SrcUri,fl(Code)),
	    ctpl(lbl("()2",2),[strg("*"),
			       lst([SrcTerm,CodeTerm,SigTerm])])) :-
  showUri(SrcUri,U,[]),
  string_chars(U,C),
  mkDetail("source",C,SrcTerm),
  mkDetail("code",Code,CodeTerm),
  mkDetail("signature",Sig,SigTerm).
versionTerm((ver(V),Sig,SrcUri,fl(Code)),
	    ctpl(lbl("()2",2),[strg(V),
			       lst([SrcTerm,CodeTerm,SigTerm])])) :-
  showUri(SrcUri,U,[]),
  string_chars(U,C),
  mkDetail("source",C,SrcTerm),
  mkDetail("code",Code,CodeTerm),
  mkDetail("signature",Sig,SigTerm).

mkDetail(Nm,Vl,ctpl(lbl(Nm,1),[strg(Vl)])).

writeManifest(Fn,M) :-
  manifestTerm(M,Term),
  encode(Term,Text),
  writeFile(Fn,Text).

dispManifest(M) :-
  showManifest(M,Chrs,[]),
  string_chars(Txt,Chrs),
  writeln(Txt).

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

showVersion((V,Sig,U,F),O,Ox) :-
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

