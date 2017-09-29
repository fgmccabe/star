:- module(plog,[displayPlRules/1]).

:- use_module(misc).
:- use_module(types).

displayPlRules(export(_,Imports,Fields,Types,Classes,Rules,Contracts,Impls)) :-
  showImports(Imports,Chrs,O1),
  showType(faceType(Fields),O1,O2),
  appStr(".\n",O2,O3),
  showTypeDefs(Types,O3,O4),
  showContracts(Contracts,O4,O5),
  showClasses(Classes,O5,O6),
  showPlRules(Rules,O6,O7),
  showImpls(Impls,O7,[]),
  string_chars(Text,Chrs),
  write(Text).

showImports([],O,O).
showImports([I|More],O,Ox) :-
  showImport(I,O,O1),
  showImports(More,O1,Ox).

showImport(import(Viz,pkg(Pkg,_),_,_,_,_,_),O,Ox) :-
  showViz(Viz,O,O1),
  appStr(Pkg,O1,O2),
  appStr(".\n",O2,Ox).

showViz(private,O,Ox) :-
  appStr("private import ",O,Ox).
showViz(public,O,Ox) :-
  appStr("public import ",O,Ox).

showTypeDefs(L,O,Ox) :-
  listShow(L,plog:showTypeDef,"\n",O,Ox).

showTypeDef((_,Type),O,Ox) :-
  showType(Type,O,Ox).

showContracts(Cons,O,Ox) :-
  listShow(Cons,plog:showContract,".\n",O,Ox).

showContract(contract(LclNm,Nm,_,Con,Mtds),O,Ox) :-
  appStr("contract: ",O,O0),
  appStr(LclNm,O0,O1),
  appStr("@",O1,O2),
  appStr(Nm,O2,O3),
  appStr(":",O3,O4),
  showConstraint(Con,O4,O5),
  appStr(" <~ ",O5,O6),
  showType(Mtds,O6,O7),
  appStr(".\n",O7,Ox).

showClasses([],O,O).
showClasses([(Nm,Access,Tp)|Cl],O,Ox) :-
  showClass(Nm,Access,Tp,O,O1),
  showClasses(Cl,O1,Ox).

showClass(Nm,Access,Tp,O,Ox) :-
  appStr("class: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr("@",O4,O5),
  showTerm(Access,O5,Ox).

showPlRules(Rls,O,Ox) :-
  rfold(Rls,plog:showPlRule,O,Ox).

showPlRule(clse(Q,Nm,Args,Body),O,Ox) :-
  showQuants(Q,O,O1),
  showTerm(Nm,O1,O2),
  appStr("(",O2,O3),
  showTerms(Args,O3,O4),
  appStr("):-",O4,O5),
  showBody(Body,O5,O6),
  appStr(".\n",O6,Ox).

showQuants([],O,O) :- !.
showQuants([V|Q],O,Ox) :-
  appStr("all ",O,O1),
  showTerm(V,O1,O2),
  rfold(Q,plog:showMoreQuant,O2,O3),
  appStr(" ~~ ",O3,Ox).

showMoreQuant(V,O,Ox) :-
  appStr(", ",O,O0),
  showTerm(V,O0,Ox).

showBody([],O,O).
showBody([G|Body],O,Ox) :-
  showGoal(G,O,O0),
  rfold(Body,plog:showMoreGoal,O0,Ox).

showMoreGoal(G,O,Ox) :-
  appStr(", ",O,O0),
  showGoal(G,O0,Ox).

showGoal(call(Pr,Args),O,Ox) :-
  showTerm(Pr,O,O0),
  appStr("(",O0,O1),
  showTerms(Args,O1,O2),
  appStr(")",O2,Ox).
showGoal(ecall(Pr,Args),O,Ox) :-
  appStr("escape ",O,O1),
  appStr(Pr,O1,O2),
  appStr("(",O2,O3),
  showTerms(Args,O3,O4),
  appStr(")",O4,Ox).
showGoal(ocall(Pr,Lbl,This),O,Ox) :-
  appStr("ocall(",O,O1),
  showTerm(Pr,O1,O2),
  appStr(",",O2,O3),
  showTerm(Lbl,O3,O4),
  appStr(",",O4,O5),
  showTerm(This,O5,O6),
  appStr(")",O6,Ox).
showGoal(neck,O,Ox) :-
  appStr("!",O,Ox).
showGoal(fail,O,Ox) :-
  appStr("fail",O,Ox).
showGoal(unify(L,R),O,Ox) :-
  showTerm(L,O,O1),
  appStr(" = ",O1,O2),
  showTerm(R,O2,Ox).
showGoal(match(L,R),O,Ox) :-
  showTerm(L,O,O1),
  appStr(" .= ",O1,O2),
  showTerm(R,O2,Ox).
showGoal(isTrue(_,R),O,Ox) :-
  appStr("istrue ",O,O2),
  showTerm(R,O2,Ox).
showGoal(raise(T),O,Ox) :-
  appStr("raise ",O,O2),
  showTerm(T,O2,Ox).

showTerm(prg(Nm,Ar),O,Ox) :-
  appStr(Nm,O,O0),
  appStr("/",O0,O1),
  appInt(Ar,O1,Ox).
showTerm(strct(Nm,Ar),O,Ox) :-
  appStr("[",O,O0),
  appStr(Nm,O0,O1),
  appStr("/",O1,O2),
  appInt(Ar,O2,O3),
  appStr("]",O3,Ox).
showTerm(intgr(Ix),O,Ox) :-
  appInt(Ix,O,Ox).
showTerm(float(Dx),O,Ox) :-
  appFlt(Dx,O,Ox).
showTerm(strg(Str),O,Ox) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showTerm(enum(Nm),O,Ox) :-
  appStr(Nm,O,Ox).
showTerm(idnt(Nm),O,Ox) :-
  appStr("?",O,O0),
  appStr(Nm,O0,Ox).
showTerm(cons(Op,Args),O,Ox) :-
  showTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,Ox).
showTerm(tpl(Args),O,Ox) :-
  appStr("(",O,O1),
  showTerms(Args,O1,O2),
  appStr(")",O2,Ox).
showTerm(anon,O,Ox) :-
  appStr("_",O,Ox).

showTerms(Terms,O,Ox) :-
  listShow(Terms,plog:showTerm,", ",O,Ox).

showImpls(L,O,Ox) :-
  listShow(L,canon:showImpl,"",O,Ox).

showImpl(imp(ImplName,Spec),O,Ox) :-
  appStr("implementation: ",O,O1),
  appStr(ImplName,O1,O2),
  appStr(" for ",O2,O3),
  showConstraint(Spec,O3,O4),
  appStr("\n",O4,Ox).
