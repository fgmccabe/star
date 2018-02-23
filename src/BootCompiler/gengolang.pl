:- module(gengolang,[genGoLang/2]).

% Generate go-lang code

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).
:- use_module(template).

genGoLang(mdule(Pkg,Imports,_Face,_Enums,Defs,_Contracts,_Impls),Text) :-
  genGoPkgName(Pkg,[],V0),
  genGoImports(Imports,V0,V1),
  genGoDefs(Defs,V1,Vx),
  processTemplateFile("internal://Go/pkg.go.plate",Vx,Chrs),
  string_chars(Text,Chrs).

genGoPkgName(pkg(Nm,_),C,[("pkgName",GoNm)|C]) :-
  makeGoPkgNm(Nm,GoNm).

makeGoPkgNm(Nm,GoNm) :-
  string_chars(Nm,Chrs),
  replace(Chrs,'.','_',RChrs),
  string_chars(GoNm,RChrs).

genGoImports([],V,V).

genGoDefs(Defs,O,Ox) :-
  rfold(Defs,gengolang:genGoDef,O,Ox).

genGoDef(fnDef(_,Nm,_,[eqn(_,Args,Value)]),O,[Txt|O]) :-
  genNm(Nm,"Name",[],D0),
  genArgDefs(Args,D0,D1),
  genTerm(Value,D1,Dx),
  processTemplateFile("internal://Go/func.go.plate",Dx,Chrs),
  string_chars(Txt,Chrs).

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
genTerm(ctpl(Op,Args),O,Ox) :-
  (isTupleStrct(Op) -> O=O1 ;  genTerm(Op,O,O1)),
  appStr("(",O1,O2),
  genTerms(Args,O2,O3),
  appStr(")",O3,Ox).

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
