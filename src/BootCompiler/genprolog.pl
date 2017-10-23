:- module(genprolog,[genRules/2]).

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genRules(export(Pkg,Imports,Fields,Types,Enums,Rules,Contracts,Impls),Text) :-
  genPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Chrs,O1),
  genPlRules(Rules,O1,[]),
  string_chars(Text,Chrs).

genPkgSig(Pkg,Imports,Fields,Enums,Contracts,Impls,O,Ox) :-
  constructPkgSig(Pkg,Imports,Fields,Enums,Contracts,Impls,Term),
  appQuoted("#pkg",'''',O,O1),
  appStr("(",O1,O2),
  encodeTerm(Term,TChrs,[]),
  appQuoted(TChrs,'"',O2,O3),
  appStr(").\n",O3,Ox).

constructPkgSig(Pkg,Imports,Fields,Enums,Contracts,Impls,
    tpl([PkgNm,tpl(Imps),FTps,tpl(ClsSigs),tpl(ConSigs),tpl(ImplSigs)])) :-
  encPkg(Pkg,PkgNm),
  encImports(Imports,Imps),
  encType(Fields,FTps),
  formatEnums(Enums,ClsSigs),
  formatContracts(Contracts,ConSigs),
  formatImpls(Impls,ImplSigs).

encPkg(pkg(Nm,Vers),cons(strct("pkg",2),[strg(Nm),V])) :-
  encVer(Vers,V).

encVer(defltVersion,enum("*")).
encVer(v(V),strg(V)).

encImports([],[]).
encImports([I|M],[IP|L]) :-
  encImport(I,IP),
  encImports(M,L).

encImport(import(Viz,Pkg,_,_,_,_,_),cons(strct("import",2),[enum(Viz),Enc])) :-
  encPkg(Pkg,Enc).

formatEnums([],[]).
formatEnums([Nm|M],[strg(Nm)|R]) :-
  formatEnums(M,R).

formatContracts([],[]).
formatContracts([contract(Nm,CnNm,Spec)|M],[tpl([strg(Nm),strg(CnNm),strg(CSig)])|R]) :-
  encodeType(Spec,CChars,[]),
  string_chars(CSig,CChars),
  formatContracts(M,R).

formatImpls([],[]).
formatImpls([imp(Nm,Spec)|M],[tpl([strg(Nm),strg(Sig)])|R]) :-
  encodeConstraint(Spec,Chars,[]),
  string_chars(Sig,Chars),
  formatImpls(M,R).

genPlRules(Rls,O,Ox) :-
  rfold(Rls,genprolog:genPlRule,O,Ox).

genPlRule(clse(_,Nm,Args,Body),O,Ox) :-
  genTerm(Nm,O,O2),
  appStr("(",O2,O3),
  genTerms(Args,O3,O4),
  appStr(")",O4,O5),
  genBody(Body,O5,O6),
  appStr(".\n",O6,Ox).

genQuants([],O,O) :- !.
genQuants([V|Q],O,Ox) :-
  appStr("all ",O,O1),
  genTerm(V,O1,O2),
  rfold(Q,genprolog:genMoreQuant,O2,O3),
  appStr(" ~~ ",O3,Ox).

genMoreQuant(V,O,Ox) :-
  appStr(", ",O,O0),
  genTerm(V,O0,Ox).

genBody([],O,O).
genBody([G|Body],O,Ox) :-
  appStr(" :- ",O,O0),
  genGoal(G,O0,O1),
  rfold(Body,genprolog:genMoreGoal,O1,Ox).

genMoreGoal(G,O,Ox) :-
  appStr(",\n    ",O,O0),
  genGoal(G,O0,Ox).

genGoal(call(Pr,Args),O,Ox) :-
  genTerm(Pr,O,O0),
  appStr("(",O0,O1),
  genTerms(Args,O1,O2),
  appStr(")",O2,Ox).
genGoal(ecall(Escape,Args),O,Ox) :-
  genQuoted(Escape,O,O0),
  appStr("(",O0,O1),
  genTerms(Args,O1,O2),
  appStr(")",O2,Ox).
genGoal(ocall(Pr,Lbl,This),O,Ox) :-
  appStr("ocall(",O,O1),
  genTerm(Pr,O1,O2),
  appStr(",",O2,O3),
  genTerm(Lbl,O3,O4),
  appStr(",",O4,O5),
  genTerm(This,O5,O6),
  appStr(")",O6,Ox).
genGoal(neck,O,Ox) :-
  appStr("!",O,Ox).
genGoal(fail,O,Ox) :-
  appStr("fail",O,Ox).
genGoal(unify(L,R),O,Ox) :-
  genTerm(L,O,O1),
  appStr(" = ",O1,O2),
  genTerm(R,O2,Ox).
genGoal(match(L,R),O,Ox) :-
  genTerm(L,O,O1),
  appStr(" = ",O1,O2),
  genTerm(R,O2,Ox).
genGoal(isTru(E),O,Ox) :-
  appStr("'lo.core@true'(",O,O1),
  genTerm(E,O1,O2),
  appStr(")",O2,Ox).
genGoal(raise(T),O,Ox) :-
  appStr("raise_exception",O,O0),
  appStr("(",O0,O1),
  genTerm(T,O1,O2),
  appStr(")",O2,Ox).

genTerm(prg(Nm,_),O,Ox) :-
  appQuoted(Nm,'''',O,Ox).
genTerm(strct(Nm,_),O,Ox) :-
  genQuoted(Nm,O,Ox).
genTerm(intgr(Ix),O,Ox) :-
  appInt(Ix,O,Ox).
genTerm(float(Dx),O,Ox) :-
  appFlt(Dx,O,Ox).
genTerm(strg(Str),O,Ox) :-
  appQuoted(Str,'"',O,Ox).
genTerm(anon,O,Ox) :-
  appStr("_",O,Ox).
genTerm(enum(Nm),O,Ox) :-
  appQuoted(Nm,'''',O,Ox).
genTerm(idnt(Nm),O,Ox) :-
  appStr("X",O,O0),
  genVar(Nm,O0,Ox).
genTerm(cons(Op,Args),O,Ox) :-
  genTerm(Op,O,O1),
  appStr("(",O1,O2),
  genTerms(Args,O2,O3),
  appStr(")",O3,Ox).
genTerm(tpl([]),O,Ox) :-!,
  appQuoted("()","'",O,Ox).
genTerm(tpl(Args),O,Ox) :-
  appStr("(",O,O5),
  genTerms(Args,O5,O6),
  appStr(")",O6,Ox).

genVar(Nm,O,Ox) :-
  string_chars(Nm,Chars),
  filterChars(Chars,O,Ox).

filterChars([],O,O).
filterChars([Ch|M],[Ch|O],Ox) :-
  isLetter(Ch),
  filterChars(M,O,Ox).
filterChars([Ch|M],['_'|O],Ox) :-
  \+isLetter(Ch),
  filterChars(M,O,Ox).

isLetter('_').
isLetter(Ch) :- char_type(Ch,alnum).

genQuoted(Str,O,Ox) :-
  appQuoted(Str,'''',O,Ox).

genTerms([],O,O).
genTerms([T|Rest],O,Ox) :-
  genTerm(T,O,O0),
  rfold(Rest,genprolog:genMoreTerm,O0,Ox).

genMoreTerm(G,O,Ox) :-
  appStr(", ",O,O0),
  genTerm(G,O0,Ox).
