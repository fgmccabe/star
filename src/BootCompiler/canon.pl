:- module(canon,[displayCanon/1,dispCanonTerm/1,dispProg/1,dispDefs/1,
    showCanon/3,showCanonTerm/3,showPkg/3,showImports/3,showTypeDefs/3,showContracts/3,
    showImpls/3,
    typeOfCanon/2,splitPtn/4,locOfCanon/2,
    isCanon/1,isSimpleCanon/1,isAssertion/1,isShow/1,isPkg/1,ruleArity/2,
    thetaDefs/2,thetaSig/2]).

:- use_module(misc).
:- use_module(operators).
:- use_module(types).
:- use_module(location).
:- use_module(uri).

isCanon(prog(_,_,_,_,_)).
isCanon(v(_,_,_)).
isCanon(over(_,_,_,_)).
isCanon(mtd(_,_,_)).
isCanon(intLit(_,_)).
isCanon(floatLit(_,_)).
isCanon(stringLit(_,_)).
isCanon(apply(_,_,_,_)).
isCanon(dot(_,_,_,_)).
isCanon(enm(_,_,_)).
isCanon(cons(_,_,_)).
isCanon(theta(_,_,_,_,_,_,_)).
isCanon(record(_,_,_,_,_,_)).
isCanon(where(_,_,_)).
isCanon(conj(_,_,_)).
isCanon(disj(_,_,_)).
isCanon(cond(_,_,_,_,_)).
isCanon(match(_,_,_)).
isCanon(neg(_,_)).
isCanon(varRef(_,_)).
isCanon(assign(_,_,_)).
isCanon(cell(_,_)).

isSimpleCanon(v(_,_,_)).
isSimpleCanon(intLit(_,_)).
isSimpleCanon(floatLit(_,_)).
isSimpleCanon(stringLit(_,_)).

isAssertion(assertion(_,_)).
isShow(show(_,_)).

isPkg(pkg(_,_)).

typeOfCanon(v(_,_,Tp),Tp) :- !.
typeOfCanon(intLit(_,Tp),Tp) :- !.
typeOfCanon(floatLit(_,Tp),Tp) :- !.
typeOfCanon(stringLit(_,Tp),Tp) :- !.
typeOfCanon(enm(_,_,Tp),Tp) :- !.
typeOfCanon(where(_,T,_),Tp) :- !, typeOfCanon(T,Tp).
typeOfCanon(abstraction(_,_,_,_,Tp),Tp) :- !.
typeOfCanon(search(_,_,_,_),tipe("star.core*boolean")) :-!.
typeOfCanon(match(_,_,_),tipe("star.core*boolean")) :-!.
typeOfCanon(conj(_,_,_),tipe("star.core*boolean")) :-!.
typeOfCanon(disj(_,_,_),tipe("star.core*boolean")) :-!.
typeOfCanon(cond(_,_,_,_,Tp),Tp) :-!.
typeOfCanon(theta(_,_,_,_,_,_,Tp),Tp) :-!.
typeOfCanon(record(_,_,_,_,_,_,Tp),Tp) :-!.
typeOfCanon(letExp(_,_,Bnd),Tp) :- !,typeOfCanon(Bnd,Tp).
typeOfCanon(apply(_,_,_,Tp),Tp) :-!.
typeOfCanon(tple(_,Els),tupleType(Tps)) :-!,
  map(Els,canon:typeOfCanon,Tps).
typeOfCanon(varRef(_,Inn),Tp) :-
  typeOfCanon(Inn,refType(Tp)).
typeOfCanon(assign(_,_,Vl),Tp) :-
  typeOfCanon(Vl,Tp).
typeOfCanon(cell(_,Vl),refType(Tp)) :-
  typeOfCanon(Vl,Tp).

locOfCanon(v(Lc,_,_),Lc) :- !.
locOfCanon(intLit(Lc,_,_),Lc) :- !.
locOfCanon(floatLit(Lc,_,_),Lc) :- !.
locOfCanon(stringLit(Lc,_,_),Lc) :- !.
locOfCanon(enm(Lc,_,_),Lc) :- !.
locOfCanon(where(Lc,_,_),Lc) :- !.
locOfCanon(abstraction(Lc,_,_,_,_),Lc) :- !.
locOfCanon(search(Lc,_,_,_),Lc) :-!.
locOfCanon(match(Lc,_,_),Lc) :-!.
locOfCanon(conj(Lc,_,_),Lc) :-!.
locOfCanon(disj(Lc,_,_),Lc) :-!.
locOfCanon(cond(Lc,_,_,_,_),Lc) :-!.
locOfCanon(theta(Lc,_,_,_,_,_,_),Lc) :-!.
locOfCanon(record(Lc,_,_,_,_,_,_),Lc) :-!.
locOfCanon(letExp(Lc,_,_),Lc) :- !.
locOfCanon(apply(Lc,_,_,_),Lc) :-!.
locOfCanon(tple(Lc,_),Lc) :-!.
locOfCanon(varRef(Lc,_),Lc) :-!.
locOfCanon(lambda(Lc,_,_),Lc) :-!.

thetaDefs(theta(_,_,_,Defs,_,_,_),Defs).
thetaDefs(record(_,_,_,Defs,_,_,_),Defs).

thetaSig(theta(_,_,_,_,_,_,Sig),Sig).
thetaSig(record(_,_,_,_,_,_,Sig),Sig).

displayCanon(Term) :- showCanon(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

dispCanonTerm(Term) :- showCanonTerm(Term,Chrs,[]), string_chars(Res,Chrs), writeln(Res).

dispDefs(Defs) :- showDefs(Defs,Chrs,[]),string_chars(Res,Chrs),writeln(Res).

dispProg(Pr) :-
  writeln("Type checked code\n"),
  showCanon(Pr,Chrs,[]),
  string_chars(Res,Chrs),writeln(Res).

showCanon(prog(Pkg,_,Imports,Defs,Others,_Fields,Types,Cons,Impls),O,Ox) :-
  showPkg(Pkg,O,O1),
  appStr("{\n",O1,O2),
  showImports(Imports,O2,O3),!,
  showTypeDefs(Types,O3,O4),!,
  appStr("\n",O4,O4a),
  showContracts(Cons,O4a,O5),!,
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
  appStr(":",O,O1),
  appStr(V,O1,Ox).

showCanonTerm(v(_,Nm,_),O,Ox) :- appIden(Nm,O,Ox).
showCanonTerm(void,O,Ox) :- appStr("void",O,Ox).
showCanonTerm(intLit(Ix,_),O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(floatLit(Ix,_),O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(stringLit(Str,_),O,Ox) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showCanonTerm(apply(_,Op,Args,_),O,Ox) :-
  showCanonTerm(Op,O,O1),
  showCanonTerm(Args,O1,Ox).
showCanonTerm(dot(_,Rc,Fld,_),O,Ox) :-
  showCanonTerm(Rc,O,O1),
  appStr(".",O1,O2),
  appStr(Fld,O2,Ox).
showCanonTerm(enm(_,Nm,_),O,Ox) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,Ox).
showCanonTerm(cons(_,Nm,_),O,Ox) :-
  appStr("%",O,O1),
  appStr(Nm,O1,Ox).
showCanonTerm(theta(_,Path,_,Defs,Others,Types,_),O,Ox) :-
  appStr(Path,O,O0),
  appStr("{\n",O0,O1),
  showTypeDefs(Types,O1,O2),
  showDefs(Defs,O2,O3),
  showOthers(Others,O3,O4),
  appStr("\n}",O4,Ox).
showCanonTerm(record(_,Path,_,Defs,Others,Types,_),O,Ox) :-
  appStr(Path,O,O0),
  appStr("{.\n",O0,O1),
  showTypeDefs(Types,O1,O2),
  showDefs(Defs,O2,O3),
  showOthers(Others,O3,O4),
  appStr("\n.}",O4,Ox).
showCanonTerm(varRef(_,Inn),O,Ox) :-
  showCanonTerm(Inn,O,O1),
  appStr("!",O1,Ox).
showCanonTerm(assign(_,Vr,Vl),O,Ox) :-
  showCanonTerm(Vr,O,O1),
  appStr(":=",O1,O2),
  showCanonTerm(Vl,O2,Ox).
showCanonTerm(cell(_,Vr),O,Ox) :-
  appStr("!!",O,O1),
  showCanonTerm(Vr,O1,Ox).
showCanonTerm(letExp(_,Env,Ex),O,Ox) :-
  appStr("let ",O,O1),
  showCanonTerm(Env,O1,O2),
  appStr(" in ",O2,O3),
  showCanonTerm(Ex,O3,Ox).
showCanonTerm(lambda(_,Rles,_),O,Ox) :-
  appStr("lambda {",O,O1),
  showRls("",Rles,O1,O2),
  appStr("}",O2,Ox).
showCanonTerm(tple(_,Els),O,Ox) :-
  appStr("(",O,O1),
  showTerms(Els,O1,O2),
  appStr(")",O2,Ox).
showCanonTerm(mtd(_,Nm,_),O,Ox) :-
  appStr("°",O,O1),
  appStr(Nm,O1,Ox).
showCanonTerm(over(_,V,_,Cons),O,Ox) :-
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
showCanonTerm(cond(_,Test,Either,Or,_),O,Ox) :-
  appStr("(",O,O1),
  showCanonTerm(Test,O1,O2),
  appStr("?",O2,O3),
  showCanonTerm(Either,O3,O4),
  appStr(" | ",O4,O5),
  showCanonTerm(Or,O5,O6),
  appStr(")",O6,Ox).
showCanonTerm(match(_,P,E),O,Ox) :-
  showCanonTerm(E,O,O1),
  appStr(" =. ",O1,O2),
  showCanonTerm(P,O2,Ox).
showCanonTerm(search(_,P,S,M),O,Ox) :-
  showCanonTerm(P,O,O1),
  appStr(" in ",O1,O2),
  showCanonTerm(S,O2,O3),
  appStr(" using ",O3,O4),
  showCanonTerm(M,O4,Ox).
showCanonTerm(abstraction(_,Bound,Guard,G,_),O,Ox) :-
  appStr("{",O,O1),
  showCanonTerm(Bound,O1,O2),
  appStr("|",O2,O3),
  showCanonTerm(Guard,O3,O4),
  appStr("} using ",O4,O5),
  showCanonTerm(G,O5,Ox).
showCanonTerm(relation(_,Bound,Guard),O,Ox) :-
  appStr("{",O,O1),
  showCanonTerm(Bound,O1,O2),
  appStr("|",O2,O3),
  showCanonTerm(Guard,O3,O4),
  appStr("}",O4,Ox).
showCanonTerm(parse(_,L,R),O,Ox) :-
  showCanonTerm(L,O,O1),
  appStr(" .~ ",O1,O2),
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
  listShow(L,canon:showImport,misc:appNl,O,O1),
  appNl(O1,Ox).

showImport(import(Viz,Pkg,_,_,_,_),O,Ox) :-
  showVisibility(Viz,O,O0),
  appStr("import ",O0,O1),
  showPkg(Pkg,O1,O2),
  appStr(".",O2,Ox).

showVisibility(private,O,Ox) :-
  appStr("private ",O,Ox).
showVisibility(public,O,Ox) :-
  appStr("public ",O,Ox).
showVisibility(transitive,O,Ox) :-
  appStr("transitive ",O,Ox).

showTypeDefs(L,O,Ox) :-
  listShow(L,canon:showTypeDef,misc:appNl,O,Ox).

showTypeDef((_,Type),O,Ox) :-
  showType(Type,true,O,Ox).

showContracts(L,O,Ox) :-
  listShow(L,canon:showContract,misc:appStr(""),O,Ox).

showContract(conDef(_,Nm,ConRule),O,Ox) :-
  appStr("contract: ",O,O1),
  appStr(Nm,O1,O3),
  appStr(" : ",O3,O4),
  showType(ConRule,true,O4,O5),
  appStr(".\n",O5,Ox).

showImpls(L,O,Ox) :-
  listShow(L,canon:showImpl,misc:appStr(""),O,Ox).

showImpl(imp(ImplName,Spec),O,Ox) :-
  appStr("implementation: ",O,O1),
  appStr(ImplName,O1,O2),
  appStr(" for ",O2,O3),
  showType(Spec,true,O3,O4),
  appStr("\n",O4,Ox).

showImplementation(Lc,Nm,Spec,O,Ox) :-
  appStr("implementation: ",O,O1),
  appStr(Nm,O1,O2),
  showType(Spec,true,O2,O5),
  appStr("@",O5,O6),
  showLocation(Lc,O6,O7),
  appStr("\n",O7,Ox).

showConstraints([],Ox,Ox).
showConstraints(Cons,O,Ox) :-
  listShow(Cons,types:showConstraint,misc:appStr(","),O,O1),
  appStr("|:",O1,Ox).

showDefs(L,O,Ox) :-
  listShow(L,canon:showDef,misc:appNl,O,Ox).

showDef(funDef(Lc,Nm,ExtNm,Type,Cx,Eqns),O,Ox) :-
  appStr("function: ",O,O1),
  appIden(Nm,O1,O2),
  appStr("«",O2,O2a),
  appIden(ExtNm,O2a,O2b),
  appStr("» @ ",O2b,O3),
  showLocation(Lc,O3,O6),
  appStr("\n",O6,O7),
  showType(Type,true,O7,O8),
  showConstraints(Cx,O8,O9),
  appStr("\n",O9,O10),
  showRls(Nm,Eqns,O10,O11),
  appStr("\n",O11,Ox),!.
showDef(varDef(Lc,Nm,ExtNm,Cx,Tp,Value),O,Ox) :-
  appStr("var: ",O,O1),
  appIden(Nm,O1,O2),
  appStr("«",O2,O2a),
  appIden(ExtNm,O2a,O2b),
  appStr("» : ",O2b,O3),
  showType(Tp,true,O3,O4),
  appStr(" @ ",O4,O5),
  showLocation(Lc,O5,O6),
  showConstraints(Cx,O6,O7),
  appStr("\n",O7,O8),
  appIden(Nm,O8,O9),
  appStr(" = ",O9,O11),
  showCanonTerm(Value,O11,O12),
  appStr(".\n",O12,Ox).
showDef(cnsDef(Lc,Nm,V,Type),O,Ox) :-
  appStr("constructor: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(" @ ",O2,O3),
  showLocation(Lc,O3,O4),
  appStr("\n",O4,O5),
  showCanonTerm(V,O5,O6),
  appStr(":",O6,O7),
  showType(Type,true,O7,O8),
  appStr("\n",O8,Ox).
showDef(typeDef(Lc,Nm,_Tp,Rl),O,Ox) :-
  appStr("type: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(" @ ",O2,O5),
  showLocation(Lc,O5,O6),
  appStr("\n",O6,O7),
  showType(Rl,true,O7,O8),
  appStr("\n",O8,Ox).
showDef(conDef(_,_,_),O,O).
showDef(implDef(_,_,_,_),O,O).

showClassRules([],O,O).
showClassRules([Rl|Rules],O,Ox) :-
  showClassRule(Rl,O,O1),
  showClassRules(Rules,O1,Ox).

showClassRule(labelRule(_,_,Hd,Ots),O,Ox) :-
  showCanonTerm(Hd,O,O1),
  appStr(" {",O1,O2),
  showOthers(Ots,O2,O3),
  appStr("}.\n",O3,Ox).

showRls(Nm,Rls,O,Ox) :-
  listShow(Rls,canon:showRule(Nm),misc:appMulti([misc:appStr("."),misc:appNl]),O,Ox).

showRule(Nm,equation(_,Args,Cond,Value),O,Ox) :-!,
  appStr(Nm,O,O1),
  showCanonTerm(Args,O1,O2),
  showGuard(Cond,O2,O5),
  appStr(" => ",O5,O6),
  showCanonTerm(Value,O6,Ox).

showGuard(enm(_,"true",_),O,O) :- !.
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
showStmt(show(_,Vl),O,Ox) :-
  appStr("  show ",O,O1),
  showCanonTerm(Vl,O1,Ox).

ruleArity(equation(_,tple(_,A),_),Ar) :-
  length(A,Ar).

splitPtn(where(_,Ptn,Cond),_,Ptn,Cond) :-!.
splitPtn(Ptn,Lc,Ptn,enm(Lc,"true",tipe("core.star*boolean"))).
