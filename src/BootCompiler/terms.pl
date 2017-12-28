:- module(terms,[displayRules/1,showRules/3,substTerm/3]).

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

showRuleSet(fnDef(_,Nm,Eqns),O,Ox) :-
  appStr("Function: ",O,O0),
  showTerm(Nm,O0,O1),
  appStr("\n",O1,O2),
  showEqns(Eqns,Nm,O2,Ox).

showEqns(L,Nm,O,Ox) :-
  listShow(L,terms:showEqn(Nm),"\n",O,Ox).

showEqn(Nm,eqn(_,Args,Value),O,Ox) :-
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
showTerm(ctpl(Op,A),O,Ox) :-
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
showTerm(varNames(_,Vars,Value),O,Ox) :-
  appStr("varDebug: [",O,O0),
  showVarNames(Vars,O0,O1),
  appStr("] -> ",O1,O2),
  showTerm(Value,O2,Ox).
showTerm(case(_,T,Cases,Deflt),O,Ox) :-
  appStr("case ",O,O1),
  showTerm(T,O1,O2),
  appStr(" in {\n",O2,O3),
  showCases(Cases,O3,O4),
  appStr("}\nelse ",O4,O5),
  showTerm(Deflt,O5,Ox).
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
showTerm(error(_,Msg),O,Ox) :-
  appStr("error: ",O,O1),
  appQuoted(Msg,"\"",O1,Ox).

showTermGuard(enu("star.core#true"),Ox,Ox) :- !.
showTermGuard(T,O,Ox) :-
  appStr(" where ",O,O1),
  showTerm(T,O1,Ox).

showVarNames(Vrs,O,Ox) :-
  listShow(Vrs,terms:showVarBinding,", ",O,Ox).

showVarBinding((V,Vx),O,Ox) :-
  appStr(V,O,O1),
  appStr("/",O1,O2),
  showTerm(Vx,O2,Ox).

showCases(Cases,O,Ox) :-
  listShow(Cases,terms:showCase,"\n| ",O,Ox).

showCase((Lbl,Val),O,Ox) :-
  showTerm(Lbl,O,O1),
  appStr(": ",O1,O2),
  showTerm(Val,O2,Ox).

substTerm(_,intgr(Ix),intgr(Ix)).
substTerm(Q,idnt(Nm),Trm) :- is_member((Nm,Trm),Q),!.
substTerm(_,idnt(Nm),idnt(Nm)).
substTerm(_,float(Dx),float(Dx)).
substTerm(_,strg(Sx),strg(Sx)).
substTerm(_,enu(Nm),enu(Nm)).
substTerm(_,strct(Nm,Ar),strct(Nm,Ar)).
substTerm(_,prg(Nm,Ar),prg(Nm,Ar)).
substTerm(Q,cll(Lc,Op,Args),cll(Lc,NOp,NArgs)) :-
  substTerm(Q,Op,NOp),
  substTerms(Q,Args,NArgs).
substTerm(Q,ocall(Lc,Call,Rec),ocall(Lc,NCall,NRec)) :-
  substTerm(Q,Call,NCall),
  substTerm(Q,Rec,NRec).
substTerm(Q,ctpl(Op,Args),ctpl(NOp,NArgs)) :-
  substTerm(Q,Op,NOp),
  substTerms(Q,Args,NArgs).
substTerm(Q,ecll(Lc,Call,Args),ecll(Lc,Call,NArgs)) :-
  substTerms(Q,Args,NArgs).
substTerm(Q,tpl(Els),tpl(NEls)) :-
  substTerms(Q,Els,NEls).
substTerm(Q,whr(Lc,T,C),whr(Lc,NT,NC)) :-
  substTerm(Q,T,NT),
  substTerm(Q,C,NC).
substTerm(Q,varNames(Lc,V,T),varNames(Lc,NV,NT)) :-
  map(V,terms:substVN(Q),NV),
  substTerm(Q,T,NT).
substTerm(Q,case(Lc,T,C),case(Lc,NT,NC)) :-
  substTerm(Q,T,NT),
  map(C,terms:substCase(Q),NC).
substTerm(Q,cnj(Lc,L,R),cnj(Lc,NL,NR)) :-
  substTerm(Q,L,NL),
  substTerm(Q,R,NR).
substTerm(Q,dsj(Lc,L,R),dsj(Lc,NL,NR)) :-
  substTerm(Q,L,NL),
  substTerm(Q,R,NR).
substTerm(Q,cnd(Lc,T,L,R),cnd(Lc,NT,NL,NR)) :-
  substTerm(Q,T,NT),
  substTerm(Q,L,NL),
  substTerm(Q,R,NR).
substTerm(Q,mtch(Lc,L,R),mtch(Lc,NL,NR)) :-
  substTerm(Q,L,NL),
  substTerm(Q,R,NR).
substTerm(Q,ng(Lc,R),ng(Lc,NR)) :-
  substTerm(Q,R,NR).

substTerms(Q,Els,NEls):-
  map(Els,terms:substTerm(Q),NEls).

substVN(Q,(T,E),(T,NE)) :-
  substTerm(Q,E,NE).

substCase(Q,(T,E),(NT,NE)) :-
  substTerm(Q,T,NT),
  substTerm(Q,E,NE).
