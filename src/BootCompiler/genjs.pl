:- module(genjs,[genJs/2]).

% Generate javascript code

:- use_module(misc).
:- use_module(types).
:- use_module(transutils).
:- use_module(encode).
:- use_module(uri).

genJs(module(_Pkg,_Imports,_Fields,_Types,_Enums,Defs,_Contracts,_Impls),Text) :-
  genJsDefs(Defs,Chrs,[]),
  string_chars(Text,Chrs).

genJsDefs(Defs,O,Ox) :-
  rfold(Defs,genjs:genDef,O,Ox).

genDef(fnDef(_,Nm,[Eqn]),O,Ox) :-
  genJsFun(Rules,Nm,O,Ox).

genJsFun(Nm,eqn(_,Args,Body),O,Ox) :-
  appStr("function ",O,O1),
  genTerm(Nm,O,O2),
  appStr("(",O2,O3),
  genTerms(Args,O3,O4),
  appStr("){\n",O4,O5),
  appStr("  var _answer=undefined;\n",O5,O6),
  genBody(Body,O6,O7),
  appStr("  return _answer;",O7,O8),
  appStr("}\n",O8,Ox).

genBody(T,O,Ox) :-
  genTerm(T,genJs:appAnswer,O,O1),
  appStr(";\n",O2,Ox).

genTerm(prg(Nm,_),Cl,O,Ox) :-
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
