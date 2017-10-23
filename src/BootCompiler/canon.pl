:- module(canon,[displayType/1,displayCanon/1,dispCanonTerm/1,dispDefs/1,showCanon/3,showCanonTerm/3,isCanon/1,isAssertion/1,isShow/1]).

:- use_module(misc).
:- use_module(operators).
:- use_module(types).
:- use_module(location).
:- use_module(uri).

isCanon(prog(_,_,_,_)).
isCanon(v(_,_)).
isCanon(intLit(_)).
isCanon(floatLit(_)).
isCanon(stringLit(_)).
isCanon(apply(_,_)).
isCanon(call(_,_,_)).
isCanon(dot(_,_)).
isCanon(enum(_,_)).
isCanon(theta(_,_,_,_)).
isCanon(record(_,_,_,_)).
isCanon(where(_,_)).
isCanon(conj(_,_,_)).
isCanon(disj(_,_,_)).
isCanon(conditional(_,_,_,_)).
isCanon(match(_,_,_)).
isCanon(one(_,_)).
isCanon(neg(_,_)).
isCanon(forall(_,_)).
isCanon(over(_,_,_)).
isCanon(mtd(_,_)).

isAssertion(assertion(_,_)).

isShow(show(_,_)).

displayCanon(Term) :- showCanon(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

displayType(Tp) :- showType(Tp,Chrs,[]), string_chars(Res,Chrs), write(Res).

dispCanonTerm(Term) :- showCanonTerm(Term,Chrs,[]), string_chars(Res,Chrs), writeln(Res).

dispDefs(Defs) :- showDefs(Defs,Chrs,[]),string_chars(Res,Chrs),writeln(Res).

showCanon(prog(Pkg,Imports,Defs,Others,_Fields,Types,Cons,Impls),O,Ox) :-
  appStr(Pkg,O,O1),
  appStr("{\n",O1,O2),
  showImports(Imports,O2,O3),!,
  showTypeDefs(Types,O3,O4),!,
  showContracts(Cons,O4,O5),!,
  showImpls(Impls,O5,O6),!,
  showDefs(Defs,O6,O7),!,
  appStr("\nOthers:\n",O7,O8),
  showOthers(Others,O8,O9),!,
  appStr("}.\n",O9,Ox),!.

showCanonTerm(v(_,Nm),O,Ox) :- appStr(Nm,O,Ox).
showCanonTerm(intLit(Ix),O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(floatLit(Ix),O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(stringLit(Str),O,Ox) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showCanonTerm(apply(Op,Args),O,Ox) :-
  showCanonTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(call(_,Op,Args),O,Ox) :-
  showCanonTerm(Op,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(cons(_,Op,A),O,Ox) :-
  showCanonTerm(Op,O,O1),
  showCanonTerm(A,O1,Ox).
showCanonTerm(dot(Rc,Fld),O,Ox) :-
  showCanonTerm(Rc,O,O1),
  appStr(".",O1,O2),
  appStr(Fld,O2,Ox).
showCanonTerm(enum(_,Nm),O,Ox) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,Ox).
showCanonTerm(theta(_,Defs,Others,Types),O,Ox) :-
  appStr("{ ",O,O1),
  showTypeDefs(Types,O1,O2),
  showDefs(Defs,O2,O3),
  showOthers(Others,O3,O4),
  appStr(" }",O4,Ox).
showCanonTerm(record(_,Defs,Others,Types),O,Ox) :-
  appStr("{. ",O,O1),
  showTypeDefs(Types,O1,O2),
  showDefs(Defs,O2,O3),
  showOthers(Others,O3,O4),
  appStr(" .}",O4,Ox).
showCanonTerm(tuple(_,Els),O,Ox) :-
  appStr("(",O,O1),
  showTerms(Els,O1,O2),
  appStr(")",O2,Ox).
showCanonTerm(mtd(_,Nm),O,Ox) :-
  appStr("&",O,O1),
  appStr(Nm,O1,Ox).
showCanonTerm(over(_,V,Cons),O,Ox) :-
  showConstraints(Cons,O,O1),
  showCanonTerm(V,O1,Ox).
showCanonTerm(where(Ptn,Cond),O,Ox) :-
  appStr("(",O,O0),
  showCanonTerm(Ptn,O0,O1),
  appStr(" where ",O1,O2),
  showCanonTerm(Cond,O2,O3),
  appStr(")",O3,Ox).
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
showCanonTerm(conditional(_,Test,Either,Or),O,Ox) :-
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
showCanonTerm(phrase(_,NT,Strm),O,Ox) :-
  showNonTerminal(NT,O,O1),
  appStr("%%",O1,O2),
  showCanonTerm(Strm,O2,Ox).
showCanonTerm(phrase(_,NT,Strm,Rem),O,Ox) :-
  showCanonTerm(NT,O,O1),
  appStr("%%",O1,O2),
  showCanonTerm(Strm,O2,O3),
  appStr("~~",O3,O4),
  showCanonTerm(Rem,O4,Ox).

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

showImport(import(Viz,pkg(Pkg,Version),_,_,_,_,_),O,Ox) :-
  showVisibility(Viz,O,O0),
  appStr("import ",O0,O1),
  appStr(Pkg,O1,O2),
  showVersion(Version,O2,O3),
  appStr(".\n",O3,Ox).

showVisibility(private,O,Ox) :-
  appStr("private ",O,Ox).
showVisibility(public,O,Ox) :-
  appStr("public ",O,Ox).

showVersion(defltVersion,O,O).
showVersion(v(V),O,Ox) :-
  appStr(",",O,O1),
  appStr(V,O1,Ox).

showTypeDefs(L,O,Ox) :-
  listShow(L,canon:showTypeDef,"\n",O,Ox).

showTypeDef((_,Type),O,Ox) :-
  showType(Type,O,Ox).

showContracts(L,O,Ox) :-
  listShow(L,canon:showContract,"\n",O,Ox).

showContract(contract(LclNm,Nm,ConRule),O,Ox) :-
  appStr("contract: ",O,O0),
  appStr(LclNm,O0,O1),
  appStr("@",O1,O2),
  appStr(Nm,O2,O3),
  appStr(":",O3,O4),
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

showImplementation(implementation(Lc,_,_,Spec,Cx,Body,_Defs,Types,Others),O,Ox) :-
  appStr("implementation: ",O,O1),
  showConstraints(Cx,O1,O4),
  showConstraint(Spec,O4,O5),
  appStr("@",O5,O6),
  showLocation(Lc,O6,O7),
  appStr("\n",O7,O8),
  appStr("<= {\n",O8,O9),
  showTypeDefs(Types,O9,O10),
  showDefs(Body,O10,O11),
  showOthers(Others,O11,O12),
  appStr("}\n",O12,Ox).

showConstraints([],Ox,Ox).
showConstraints(Cons,O,Ox) :-
  listShow(Cons,types:showConstraint,",",O,O1),
  appStr("|:",O1,Ox).

showDefs(L,O,Ox) :-
  listShow(L,canon:showDef,"\n",O,Ox).

showDef(function(Lc,Nm,Type,Cx,Eqns),O,Ox) :-
  appStr("function: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  showConstraints(Cx,O6,O7),
  appStr("\n",O7,O8),
  listShow(Eqns,canon:showEq,"\n",O8,Ox),!.
showDef(defn(Lc,Nm,Cx,Cond,Tp,Value),O,Ox) :-
  appStr("var: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  showConstraints(Cx,O6,O7),
  appStr("\n",O7,O8),
  appStr(Nm,O8,O9),
  showGuard(Cond,O9,O10),
  appStr(" = ",O10,O11),
  showCanonTerm(Value,O11,O12),
  appStr(".\n",O12,Ox).
showDef(vdefn(Lc,Nm,Cx,Cond,Tp,Value),O,Ox) :-
  appStr("var: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  showConstraints(Cx,O6,O7),
  appStr("\n",O7,O8),
  appStr(Nm,O8,O9),
  showGuard(Cond,O9,O10),
  appStr(" := ",O10,O11),
  showCanonTerm(Value,O11,O12),
  appStr(".\n",O12,Ox).
showDef(enum(Lc,Nm,Type,Cx,Rules),O,Ox) :-
  appStr("enum: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showConstraints(Cx,O7,O8),
  showClassRules(Rules,O8,Ox).
showDef(class(Lc,Nm,Type,Cx,Rules),O,Ox) :-
  appStr("class: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Type,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showConstraints(Cx,O7,O8),
  showClassRules(Rules,O8,Ox).
showDef(grammar(Lc,Nm,Tp,Cx,Rules),O,Ox) :-
  appStr("grammar: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showConstraints(Cx,O7,O8),
  showGrammarRules(Rules,O8,Ox).
showDef(typeDef(Lc,Nm,Tp,Rl),O,Ox) :-
  appStr("type: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(":",O2,O3),
  showType(Tp,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showType(Rl,O7,O8),
  appStr("\n",O8,Ox).
showDef(Con,O,Ox) :-
  Con = contract(_,_,_),
  showContract(Con,O,O2),
  appStr("\n",O2,Ox).
showDef(implementation(Lc,INm,ImplName,Spec,Cx,Defs,Body,Types,Others),O,Ox) :-
  showImplementation(implementation(Lc,INm,ImplName,Spec,Cx,Defs,Body,Types,Others),O,Ox).

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

showGuard(v(_,"true"),O,O) :- !.
showGuard(C,O,Ox) :-
  appStr(" where ",O,O1),
  showCanonTerm(C,O1,Ox).

showGrammarRules([],O,O).
showGrammarRules([Rl|Rules],O,Ox) :-
  showGrammarRule(Rl,O,O1),
  showGrammarRules(Rules,O1,Ox).

showGrammarRule(grammarRule(_,Nm,Args,PB,Body),O,Ox) :-
  appStr(Nm,O,O1),
  appStr("(",O1,O2),
  showTerms(Args,O2,O3),
  appStr(") ",O3,O4),
  showPushBack(PB,O4,O5),
  appStr(" --> ",O5,O6),
  showNonTerminal(Body,O6,O7),
  appStr(".\n",O7,Ox).

showPushBack([],O,O).
showPushBack(Els,O,Ox) :-
  appStr("[",O,O1),
  showTerminals(Els,O1,O2),
  appStr("]",O2,Ox).

showTerminals([],O,O).
showTerminals([term(_,_,T)|L],O,Ox) :-
  showCanonTerm(T,O,O1),
  showMoreTerminals(L,O1,Ox).

showMoreTerminals([],O,O).
showMoreTerminals([term(_,_,T)|L],O,Ox) :-
  appStr(", ",O,O1),
  showCanonTerm(T,O1,O2),
  showMoreTerminals(L,O2,Ox).

showNonTerminal(terminals(_,Terms),O,Ox) :-
  showPushBack(Terms,O,Ox).
showNonTerminal(conj(_,Lhs,Rhs),O,Ox) :-
  showNonTerminal(Lhs,O,O1),
  appStr(" && ",O1,O2),
  showNonTerminal(Rhs,O2,Ox).
showNonTerminal(guard(_,Lhs,Rhs),O,Ox) :-
  showNonTerminal(Lhs,O,O1),
  appStr(" where ",O1,O2),
  showCanonTerm(Rhs,O2,Ox).
showNonTerminal(disj(_,Lhs,Rhs),O,Ox) :-
  appStr("(",O,O0),
  showNonTerminal(Lhs,O0,O1),
  appStr(" || ",O1,O2),
  showNonTerminal(Rhs,O2,O3),
  appStr(")",O3,Ox).
showNonTerminal(conditional(_,Test,Lhs,Rhs),O,Ox) :-
  appStr("(",O,O0),
  showNonTerminal(Test,O0,O1),
  appStr("?",O1,O2),
  showNonTerminal(Lhs,O2,O3),
  appStr(" | ",O3,O4),
  showNonTerminal(Rhs,O4,O5),
  appStr(")",O5,Ox).
showNonTerminal(neg(_,Rhs),O,Ox) :-
  appStr("(",O,O1),
  appStr("\\+",O1,O2),
  showNonTerminal(Rhs,O2,O3),
  appStr(")",O3,Ox).
showNonTerminal(ahead(_,Rhs),O,Ox) :-
  appStr("(",O,O1),
  showNonTerminal(Rhs,O1,O2),
  appStr(")+",O2,Ox).
showNonTerminal(goal(_,Rhs),O,Ox) :-
  appStr("{",O,O1),
  showCanonTerm(Rhs,O1,O2),
  appStr(")",O2,Ox).
showNonTerminal(eof(_,_),O,Ox) :-
  appStr("eof",O,Ox).
showNonTerminal(NT,O,Ox) :-
  showCanonTerm(NT,O,Ox).

showOthers([],O,O).
showOthers([Stmt|Stmts],O,Ox) :-
  showStmt(Stmt,O,O2),
  appStr(".\n",O2,O3),
  showOthers(Stmts,O3,Ox).

showStmt(assertion(_,Cond),O,Ox) :-
  appStr("  assert ",O,O1),
  showCanonTerm(Cond,O1,Ox).

showStmt(show(_,Exp),O,Ox) :-
  appStr("  show ",O,O1),
  showCanonTerm(Exp,O1,Ox).
