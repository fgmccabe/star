:- module(canon,[displayType/1,displayCanon/1,dispCanonTerm/1,dispProg/1,dispDefs/1,
    showCanon/3,showCanonTerm/3,showPkg/3,showImports/3,showTypeDefs/3,showContracts/3,
    showImpls/3,
    isCanon/1,isAssertion/1,
    thetaLoc/2,thetaDefs/2]).

:- use_module(misc).
:- use_module(operators).
:- use_module(types).
:- use_module(location).
:- use_module(uri).

isCanon(prog(_,_,_,_,_)).
isCanon(v(_,_)).
isCanon(over(_,_,_)).
isCanon(mtd(_,_)).
isCanon(intLit(_)).
isCanon(floatLit(_)).
isCanon(stringLit(_)).
isCanon(apply(_,_,_)).
isCanon(dot(_,_,_)).
isCanon(enm(_,_)).
isCanon(cns(_,_)).
isCanon(theta(_,_,_,_,_)).
isCanon(record(_,_,_,_,_)).
isCanon(where(_,_,_)).
isCanon(conj(_,_,_)).
isCanon(disj(_,_,_)).
isCanon(cond(_,_,_,_)).
isCanon(match(_,_,_)).
isCanon(neg(_,_)).

isAssertion(assertion(_,_)).

thetaLoc(theta(Lc,_,_,_,_),Lc).
thetaLoc(record(Lc,_,_,_,_),Lc).

thetaDefs(theta(_,_,Defs,_,_),Defs).
thetaDefs(record(_,_,Defs,_,_),Defs).

displayCanon(Term) :- showCanon(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

displayType(Tp) :- showType(Tp,Chrs,[]), string_chars(Res,Chrs), write(Res).

dispCanonTerm(Term) :- showCanonTerm(Term,Chrs,[]), string_chars(Res,Chrs), writeln(Res).

dispDefs(Defs) :- showDefs(Defs,Chrs,[]),string_chars(Res,Chrs),writeln(Res).

dispProg(Pr) :-
  showCanon(Pr,Chrs,[]),
  string_chars(Res,Chrs),writeln(Res).

showCanon(prog(Pkg,_,Imports,Defs,Others,_Fields,Types,Cons,Impls),O,Ox) :-
  showPkg(Pkg,O,O1),
  appStr("{\n",O1,O2),
  showImports(Imports,O2,O3),!,
  showTypeDefs(Types,O3,O4),!,
  showContracts(Cons,O4,O5),!,
  showImpls(Impls,O5,O6),!,
  showDefs(Defs,O6,O7),!,
  appStr("\nOthers:\n",O7,O8),
  showOthers(Others,O8,O9),!,
  appStr("}.\n",O9,Ox),!.

showPkg(pkg(Nm,V),O,Ox) :-
  appStr(Nm,O,O1),
  showVersion(V,O1,Ox).

showVersion(defltVersion,O,O).
showVersion(ver(V),O,Ox) :-
  appStr("#",O,O1),
  appStr(V,O1,Ox).

showCanonTerm(v(_,Nm),O,Ox) :- appStr(Nm,O,Ox).
showCanonTerm(intLit(Ix),O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(floatLit(Ix),O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(stringLit(Str),O,Ox) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showCanonTerm(apply(_,Op,Args),O,Ox) :-
  showCanonTerm(Op,O,O1),
  showCanonTerm(Args,O1,Ox).
showCanonTerm(cons(_,Op,A),O,Ox) :-
  showCanonTerm(Op,O,O1),
  showCanonTerm(A,O1,Ox).
showCanonTerm(dot(_,Rc,Fld),O,Ox) :-
  showCanonTerm(Rc,O,O1),
  appStr(".",O1,O2),
  appStr(Fld,O2,Ox).
showCanonTerm(enm(_,Nm),O,Ox) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,Ox).
showCanonTerm(cns(_,Nm),O,Ox) :-
  appStr("%",O,O1),
  appStr(Nm,O1,Ox).
showCanonTerm(theta(_,_,Defs,Others,Types),O,Ox) :-
  appStr("{ ",O,O1),
  showTypeDefs(Types,O1,O2),
  showDefs(Defs,O2,O3),
  showOthers(Others,O3,O4),
  appStr(" }",O4,Ox).
showCanonTerm(record(_,_,Defs,Others,Types),O,Ox) :-
  appStr("{. ",O,O1),
  showTypeDefs(Types,O1,O2),
  showDefs(Defs,O2,O3),
  showOthers(Others,O3,O4),
  appStr(" .}",O4,Ox).
showCanonTerm(tple(_,Els),O,Ox) :-
  appStr("(",O,O1),
  showTerms(Els,O1,O2),
  appStr(")",O2,Ox).
showCanonTerm(mtd(_,Nm),O,Ox) :-
  appStr("&",O,O1),
  appStr(Nm,O1,Ox).
showCanonTerm(over(_,V,Cons),O,Ox) :-
  showConstraints(Cons,O,O1),
  showCanonTerm(V,O1,Ox).
showCanonTerm(where(_,Ptn,Cond),O,Ox) :-
  showCanonTerm(Ptn,O,O1),
  showGuard(Cond,O1,Ox).
showCanonTerm(conj(_,L,R),O,Ox) :-
  showCanonTerm(L,O,O1),
  appStr(" && ",O1,O2),
  showCanonTerm(R,O2,Ox).
showCanonTerm(disj(_,Either,Or),O,Ox) :-
  appStr("(",O,O0),
  showCanonTerm(Either,O0,O1),
  appStr(" || ",O1,O2),
  showCanonTerm(Or,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(cond(_,Test,Either,Or),O,Ox) :-
  appStr("(",O,O1),
  showCanonTerm(Test,O1,O2),
  appStr("?",O2,O3),
  showCanonTerm(Either,O3,O4),
  appStr(" | ",O4,O5),
  showCanonTerm(Or,O5,O6),
  appStr(")",O6,Ox).
showCanonTerm(match(_,L,R),O,Ox) :-
  showCanonTerm(L,O,O1),
  appStr(" .= ",O1,O2),
  showCanonTerm(R,O2,Ox).
showCanonTerm(neg(_,R),O,Ox) :-
  appStr("\\+",O,O1),
  showCanonTerm(R,O1,Ox).

showTerms([],O,O).
showTerms([T|More],O,Ox) :-
  showCanonTerm(T,O,O1),
  showMoreTerms(More,O1,Ox).

showMoreTerms([],O,O).
showMoreTerms([T|More],O,Ox) :-
  appStr(", ",O,O1),
  showCanonTerm(T,O1,O2),
  showMoreTerms(More,O2,Ox).

showEntries([],O,O).
showEntries([(Ky,Vl)|M],O,Ox) :-
  showCanonTerm(Ky,O,O1),
  appStr(" -> ",O1,O2),
  showCanonTerm(Vl,O2,O3),
  appStr(". ",O3,O4),
  showEntries(M,O4,Ox).

showImports(L,O,Ox) :-
  listShow(L,canon:showImport,"\n",O,Ox).

showImport(import(Viz,Pkg,_,_,_,_,_),O,Ox) :-
  showVisibility(Viz,O,O0),
  appStr("import ",O0,O1),
  showPkg(Pkg,O1,O2),
  appStr(".\n",O2,Ox).

showVisibility(private,O,Ox) :-
  appStr("private ",O,Ox).
showVisibility(public,O,Ox) :-
  appStr("public ",O,Ox).

showTypeDefs(L,O,Ox) :-
  listShow(L,canon:showTypeDef,"\n",O,Ox).

showTypeDef((_,Type),O,Ox) :-
  showType(Type,O,Ox).

showContracts(L,O,Ox) :-
  listShow(L,canon:showContract,"\n",O,Ox).

showContract(conDef(LclNm,Nm,ConRule),O,Ox) :-,
  appStr(LclNm,O,O0)
  appStr("contract: ",O0,O1),
  appStr("\n",O1,O2),
  appStr(Nm,O2,O3),
  appStr(" : ",O3,O4),
  showType(ConRule,O4,O5),
  appStr(".\n",O5,Ox).

showImpls(L,O,Ox) :-
  listShow(L,canon:showImpl,"",O,Ox).

showImpl(imp(ImplName,Spec),O,Ox) :-
  appStr("implementation: ",O,O1),
  appStr(ImplName,O1,O2),
  appStr(" for ",O2,O3),
  showConstraint(Spec,O3,O4),
  appStr("\n",O4,Ox).

showImplementation(Lc,Nm,Spec,O,Ox) :-
  appStr("implementation: ",O,O1),
  appStr(Nm,O1,O2),
  showConstraint(Spec,O2,O5),
  appStr("@",O5,O6),
  showLocation(Lc,O6,O7),
  appStr("\n",O7,Ox).

showConstraints([],Ox,Ox).
showConstraints(Cons,O,Ox) :-
  listShow(Cons,types:showConstraint,",",O,O1),
  appStr("|:",O1,Ox).

showDefs(L,O,Ox) :-
  listShow(L,canon:showDef,"\n",O,Ox).

showDef(funDef(Lc,Nm,Type,Cx,Eqns),O,Ox) :-
  appStr("function: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  showConstraints(Cx,O6,O7),
  appStr("\n",O7,O8),
  listShow(Eqns,canon:showEq,"\n",O8,Ox),!.
showDef(varDef(Lc,Nm,Cx,Tp,Value),O,Ox) :-
  appStr("var: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  showConstraints(Cx,O6,O7),
  appStr("\n",O7,O8),
  appStr(Nm,O8,O9),
  appStr(" = ",O9,O11),
  showCanonTerm(Value,O11,O12),
  appStr(".\n",O12,Ox).
showDef(vdefn(Lc,Nm,Cx,Tp,Value),O,Ox) :-
  appStr("var: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  showConstraints(Cx,O6,O7),
  appStr("\n",O7,O8),
  appStr(Nm,O8,O9),
  appStr(" := ",O9,O11),
  showCanonTerm(Value,O11,O12),
  appStr(".\n",O12,Ox).
showDef(cnsDef(Lc,Nm,V,Type),O,Ox) :-
  appStr("constructor: ",O,O1),
  appStr(Nm,O1,O2),
  appStr("\n",O2,O3),
  showCanonTerm(V,O3,O4),
  appStr(":",O4,O5),
  showType(Type,O5,O6),
  appStr(" @ ",O6,O7),
  showLocation(Lc,O7,O8),
  appStr("\n",O8,Ox).
showDef(typeDef(Lc,Nm,Tp,Rl),O,Ox) :-
  appStr("type: ",O,O1),
  appStr(Nm,O1,O2),
  appStr("\n",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showType(Rl,O7,O8),
  appStr("\n",O8,Ox).
showDef(Con,O,Ox) :-
  Con = conDef(_,_,_),
  showContract(Con,O,O2),
  appStr("\n",O2,Ox).
showDef(implDef(Lc,_,ImplName,Spec),O,Ox) :-
  showImplementation(Lc,ImplName,Spec,O,Ox).

showClassRules([],O,O).
showClassRules([Rl|Rules],O,Ox) :-
  showClassRule(Rl,O,O1),
  showClassRules(Rules,O1,Ox).

showClassRule(labelRule(_,_,Hd,Ots),O,Ox) :-
  showCanonTerm(Hd,O,O1),
  appStr(" {",O1,O2),
  showOthers(Ots,O2,O3),
  appStr("}.\n",O3,Ox).

showEq(equation(_,Nm,Args,Cond,Value),O,Ox) :-
  appStr(Nm,O,O1),
  showCanonTerm(Args,O1,O2),
  showGuard(Cond,O2,O5),
  appStr(" => ",O5,O6),
  showCanonTerm(Value,O6,O7),
  appStr(".",O7,Ox).

showGuard(enm(_,"true"),O,O) :- !.
showGuard(C,O,Ox) :-
  appStr(" where ",O,O1),
  showCanonTerm(C,O1,Ox).

showOthers([],O,O).
showOthers([Stmt|Stmts],O,Ox) :-
  showStmt(Stmt,O,O2),
  appStr(".\n",O2,O3),
  showOthers(Stmts,O3,Ox).

showStmt(assertion(_,Cond),O,Ox) :-
  appStr("  assert ",O,O1),
  showCanonTerm(Cond,O1,Ox).

showStmt(ignore(_,Exp),O,Ox) :-
  appStr("  ignore ",O,O1),
  showCanonTerm(Exp,O1,Ox).
