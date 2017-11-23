:- module(terms,[displayRules/1,showRules/3]).

:- use_module(misc).
:- use_module(canon).
:- use_module(operators).
:- use_module(location).

showRules(module(Pkg,Imports,_,Types,_,Defs,Contracts,Impls),O,Ox) :-
  showPkg(Pkg,O,O1),
  appStr("{\n",O1,O2),
  showImports(Imports,O2,O3),!,
  showTypeDefs(Types,O3,O4),!,
  showContracts(Contracts,O4,O5),!,
  showImpls(Impls,O5,O6),!,
  showRuleSets(Defs,O6,O7),!,
  appStr("\n}.\n",O7,Ox),!.

displayRules(Term) :- showRules(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

showRuleSets(L,O,Ox) :-
  listShow(L,terms:showRuleSet,"\n\n",O,Ox).

showRuleSet(fnDef(_,_,Eqns),O,Ox) :-
  appStr("Function:\n",O,O1),
  listShow(Eqns,terms:showRule,"\n",O1,Ox).

showRule(eqn(_,_,Nm,Args,Value),O,Ox) :-
  showTerm(Nm,O,O1),
  showArgs(Args,O1,O2),
  appStr(" => ",O2,O3),
  showTerm(Value,O3,O4),
  appStr(".",O4,Ox).

showArgs(Args,O,Ox) :-
  showTerms(Args,"(",",",")",O,Ox).

showTerms(Terms,Lft,Mid,Rgt,O,Ox) :-
  appStr(Lft,O,O1),
  listShow(Terms,terms:showTerm,Mid,O1,O2),
  appStr(Rgt,O2,Ox).

showTerm(idnt(Nm),O,Ox) :- appStr(Nm,O,Ox).
showTerm(intgr(Ix),O,Ox) :- appInt(Ix,O,Ox).
showTerm(float(Ix),O,Ox) :- appInt(Ix,O,Ox).
showTerm(strg(Str),O,Ox) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showTerm(cll(_,Op,Args),O,Ox) :-
  showTerm(Op,O,O1),
  showArgs(Args,O1,Ox).
showTerm(ocall(_,Call,Rec),O,Ox) :-
  showTerm(Rec,O,O1),
  appStr(".",O1,O2),
  showTerm(Call,O2,Ox).
showTerm(ecll(_,Es,Args),O,Ox) :-
  appStr(Es,O,O1),
  showArgs(Args,O1,Ox).
showTerm(cns(Op,A),O,Ox) :-
  showTerm(Op,O,O1),
  showArgs(A,O1,Ox).
showTerm(enu(Nm),O,Ox) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,Ox).
showTerm(strct(Nm,Ar),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("%",O1,O2),
  appInt(Ar,O2,Ox).
showTerm(prg(Nm,Ar),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("/",O1,O2),
  appInt(Ar,O2,Ox).
showTerm(tpl(Els),O,Ox) :-
  showArgs(Els,O,Ox).
showTerm(whr(_,Ptn,Cond),O,Ox) :-
  showTerm(Ptn,O,O1),
  showTermGuard(Cond,O1,Ox).
showTerm(cnj(_,L,R),O,Ox) :-
  showTerm(L,O,O1),
  appStr(" && ",O1,O2),
  showTerm(R,O2,Ox).
showTerm(dsj(_,Either,Or),O,Ox) :-
  appStr("(",O,O0),
  showTerm(Either,O0,O1),
  appStr(" || ",O1,O2),
  showTerm(Or,O2,O3),
  appStr(")",O3,Ox).
showTerm(cnd(_,Test,Either,Or),O,Ox) :-
  appStr("(",O,O1),
  showTerm(Test,O1,O2),
  appStr("?",O2,O3),
  showTerm(Either,O3,O4),
  appStr(" | ",O4,O5),
  showTerm(Or,O5,O6),
  appStr(")",O6,Ox).
showTerm(mtch(_,L,R),O,Ox) :-
  showTerm(L,O,O1),
  appStr(" .= ",O1,O2),
  showTerm(R,O2,Ox).
showTerm(ng(_,R),O,Ox) :-
  appStr("\\+",O,O1),
  showTerm(R,O1,Ox).

showTermGuard(enu("star.core#true"),Ox,Ox) :- !.
showTermGuard(T,O,Ox) :-
  appStr(" where ",O,O1),
  showTerm(T,O1,Ox).
