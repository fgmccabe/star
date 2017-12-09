:- module(genprolog,[genRules/3]).

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genRules(module(Pkg,Imports,Fields,Types,Enums,Defs,Contracts,Impls),Sig,Text) :-
  genPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Sig),
  genPlDefs(Defs,Chrs,[]),
  string_chars(Text,Chrs).

genPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Sig) :-
  constructPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,Term),
  encodeTerm(Term,Chrs,[]),
  string_chars(Sig,Chrs).

constructPkgSig(Pkg,Imports,Fields,Types,Enums,Contracts,Impls,
    tpl([PkgNm,tpl(Imps),strg(Sig),tpl(ClsSigs),tpl(ConSigs),tpl(ImplSigs)])) :-
  encPkg(Pkg,PkgNm),
  encImports(Imports,Imps),
  encType(faceType(Fields,Types),Sig),
  formatEnums(Enums,ClsSigs),
  formatContracts(Contracts,ConSigs),
  formatImpls(Impls,ImplSigs).

encPkg(pkg(Nm,Vers),cons(strct("pkg",2),[strg(Nm),V])) :-
  encVer(Vers,V).

encVer(defltVersion,enum("*")).
encVer(ver(V),strg(V)).

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
formatContracts([conDef(Nm,CnNm,Spec)|M],[tpl([strg(Nm),strg(CnNm),strg(CSig)])|R]) :-
  encodeType(Spec,CChars,[]),
  string_chars(CSig,CChars),
  formatContracts(M,R).

formatImpls([],[]).
formatImpls([imp(Nm,Spec)|M],[tpl([strg(Nm),strg(Sig)])|R]) :-
  encodeConstraint(Spec,Chars,[]),
  string_chars(Sig,Chars),
  formatImpls(M,R).

genPlDefs(Defs,O,Ox) :-
  rfold(Defs,genprolog:genDef,O,Ox).

genDef(fnDef(_,Nm,Rules),O,Ox) :-
  genPlRules(Rules,Nm,O,Ox).

genPlRules(Rls,Nm,O,Ox) :-
  rfold(Rls,genprolog:genPlRule(Nm),O,Ox).

genPlRule(Nm,eqn(_,Args,Body),O,Ox) :-
  genTerm(Nm,O,O2),
  appStr("(",O2,O3),
  genTerms(Args,O3,O4),
  appStr(")",O4,O5),
  genBody(Body,O5,O6),
  appStr(".\n",O6,Ox).

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
  appStr("'star.core@true'(",O,O1),
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
