:- module(canon,[dispCanonTerm/1,dispProg/1,dispDefs/1,
		 showCanonTerm/4,showPkg/3,showImports/3,showContracts/3,
		 showImpls/3,
		 typeOfCanon/2,splitPtn/3,locOfCanon/2,mergeGl/4,
		 isCanon/1,isSimpleCanon/1,isAssertion/1,isShow/1,isPkg/1,
		 ruleArity/2,
		 isGoal/1,isIterableGoal/1,
		 anonVar/3]).

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
isCanon(tple(_,_)).
isCanon(theta(_,_,_,_,_,_,_)).
isCanon(record(_,_,_,_,_,_)).
isCanon(where(_,_,_)).
isCanon(conj(_,_,_)).
isCanon(disj(_,_,_)).
isCanon(implies(_,_,_)).
isCanon(cond(_,_,_,_,_)).
isCanon(match(_,_,_)).
isCanon(neg(_,_)).
isCanon(varRef(_,_)).
isCanon(assign(_,_,_)).
isCanon(cell(_,_)).
isCanon(lambda(_,_,_)).
isCanon(doTerm(_,_,_,_,_)).
isCanon(noDo(_)).
isCanon(seqDo(_,_,_)).
isCanon(ifThenDo(_,_,_,_,_,_,_)).
isCanon(whileDo(_,_,_,_,_)).
isCanon(forDo(_,_,_,_,_)).
isCanon(tryCatchDo(_,_,_,_,_,_)).
isCanon(varDo(_,_,_)).
isCanon(bindDo(_,_,_,_)).
isCanon(returnDo(_,_,_,_)).
isCanon(throwDo(_,_,_,_,_)).
isCanon(performDo(_,_,_,_,_)).
isCanon(simpleDo(_,_,_)).
isCanon(search(_,_,_,_)).

isSimpleCanon(v(_,_,_)).
isSimpleCanon(intLit(_,_)).
isSimpleCanon(floatLit(_,_)).
isSimpleCanon(stringLit(_,_)).

isAssertion(assertion(_,_)).
isShow(show(_,_)).

isGoal(match(_,_,_)) :-!.
isGoal(conj(_,_,_)) :- !.
isGoal(implies(_,_,_)) :- !.
isGoal(disj(_,_,_)) :- !.
isGoal(neg(_,_)) :- !.
isGoal(search(_,_,_,_)) :- !.

isAction(seqDo(_,_,_)).
isAction(ifThenDo(_,_,_,_,_,_,_)).
isAction(whileDo(_,_,_,_,_)).
isAction(forDo(_,_,_,_,_)).
isAction(tryCatch(_,_,_,_,_)).
isAction(assign(_,_,_)).
isAction(apply(_,_,_,_)).
isAction(bindDo(_,_,_,_)).
isAction(varDo(_,_,_)).
isAction(returnDo(_,_,_,_)).
isAction(throwDo(_,_,_,_,_)).
isAction(performDo(_,_,_,_,_)).
isAction(noDo(_)).

isIterableGoal(conj(_,L,R)) :- !, (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(implies(_,L,R)) :- !, (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(disj(_,L,R)) :- !,  (isIterableGoal(L) ; isIterableGoal(R)).
isIterableGoal(neg(_,R)) :- !, isIterableGoal(R).
isIterableGoal(search(_,_,_,_)) :- !.

isPkg(pkg(_,_)).

typeOfCanon(v(_,_,Tp),Tp) :- !.
typeOfCanon(dot(_,_,_,Tp),Tp) :- !.
typeOfCanon(intLit(_,Tp),Tp) :- !.
typeOfCanon(floatLit(_,Tp),Tp) :- !.
typeOfCanon(stringLit(_,Tp),Tp) :- !.
typeOfCanon(enm(_,_,Tp),Tp) :- !.
typeOfCanon(where(_,T,_),Tp) :- !, typeOfCanon(T,Tp).
typeOfCanon(abstraction(_,_,_,_,_,Tp),Tp) :- !.
typeOfCanon(search(_,_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(match(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(conj(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(disj(_,_,_),type("star.core*boolean")) :-!.
typeOfCanon(implies(_,_,_),type("star.core*boolean")) :-!.
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
typeOfCanon(lambda(_,_,Tp),Tp) :-!.
typeOfCanon(over(_,T,_,_),Tp) :- typeOfCanon(T,Tp).
typeOfCanon(mtd(_,_,Tp),Tp) :-!.

locOfCanon(v(Lc,_,_),Lc) :- !.
locOfCanon(dot(Lc,_,_,_),Lc) :- !.
locOfCanon(intLit(Lc,_),Lc) :- !.
locOfCanon(floatLit(Lc,_),Lc) :- !.
locOfCanon(stringLit(Lc,_),Lc) :- !.
locOfCanon(enm(Lc,_,_),Lc) :- !.
locOfCanon(where(Lc,_,_),Lc) :- !.
locOfCanon(abstraction(Lc,_,_,_,_,_),Lc) :- !.
locOfCanon(search(Lc,_,_,_),Lc) :-!.
locOfCanon(match(Lc,_,_),Lc) :-!.
locOfCanon(conj(Lc,_,_),Lc) :-!.
locOfCanon(disj(Lc,_,_),Lc) :-!.
locOfCanon(implies(Lc,_,_),Lc) :-!.
locOfCanon(cond(Lc,_,_,_,_),Lc) :-!.
locOfCanon(theta(Lc,_,_,_,_,_,_),Lc) :-!.
locOfCanon(record(Lc,_,_,_,_,_,_),Lc) :-!.
locOfCanon(letExp(Lc,_,_),Lc) :- !.
locOfCanon(case(Lc,_,_,_),Lc) :- !.
locOfCanon(apply(Lc,_,_,_),Lc) :-!.
locOfCanon(tple(Lc,_),Lc) :-!.
locOfCanon(varRef(Lc,_),Lc) :-!.
locOfCanon(lambda(Lc,_,_),Lc) :-!.
locOfCanon(doTerm(Lc,_,_,_,_),Lc) :-!.
locOfCanon(seqDo(Lc,_,_),Lc) :-!.
locOfCanon(ifThenDo(Lc,_,_,_,_,_,_),Lc) :-!.
locOfCanon(whileDo(Lc,_,_,_,_),Lc) :-!.
locOfCanon(forDo(Lc,_,_,_,_),Lc) :-!.
locOfCanon(tryCatchDo(Lc,_,_,_,_,_),Lc) :-!.
locOfCanon(assign(Lc,_,_,_,_),Lc) :-!.
locOfCanon(apply(Lc,_,_,_),Lc) :-!.
locOfCanon(delayDo(Lc,_,_,_,_),Lc) :-!.
locOfCanon(bindDo(Lc,_,_,_),Lc) :-!.
locOfCanon(varDo(Lc,_,_),Lc) :-!.
locOfCanon(returnDo(Lc,_,_,_),Lc) :-!.
locOfCanon(throwDo(Lc,_,_,_,_),Lc) :-!.
locOfCanon(performDo(Lc,_,_,_,_),Lc) :-!.
locOfCanon(simpleDo(Lc,_,_),Lc) :-!.
locOfCanon(noDo(Lc),Lc) :-!.

dispCanonTerm(Term) :-
  showCanonTerm(Term,0,Chrs,[]),
  string_chars(Res,Chrs), writeln(Res).

dispDefs(Defs) :- showDefs(Defs,0,Chrs,[]),string_chars(Res,Chrs),writeln(Res).

dispProg(Pr) :-
  writeln("Type checked code\n"),
  showCanonProg(Pr,Chrs,[]),
  string_chars(Res,Chrs),writeln(Res).

showCanonProg(prog(Pkg,_,Imports,Defs,Others,_Fields,Types,Cons,Impls),O,Ox) :-
  showPkg(Pkg,O,O1),
  appStr("{\n",O1,O2),
  showImports(Imports,O2,O3),!,
  showTypeDefs(Types,0,O3,O4),!,
  appStr("\n",O4,O4a),
  showContracts(Cons,O4a,O5),!,
  showImpls(Impls,O5,O6),!,
  showDefs(Defs,0,O6,O7),!,
  appStr("\nOthers:\n",O7,O8),
  showOthers(Others,0,O8,O9),!,
  appStr("}.\n",O9,Ox),!.

showPkg(pkg(Nm,V),O,Ox) :-
  appStr(Nm,O,O1),
  showVersion(V,O1,Ox).

showVersion(defltVersion,O,O).
showVersion(ver(V),O,Ox) :-
  appStr(":",O,O1),
  appStr(V,O1,Ox).

showCanonTerm(v(_,Nm,_),_,O,Ox) :- appIden(Nm,O,Ox).
showCanonTerm(void,_,O,Ox) :- appStr("void",O,Ox).
showCanonTerm(intLit(Ix,_),_,O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(floatLit(Ix,_),_,O,Ox) :- appInt(Ix,O,Ox).
showCanonTerm(stringLit(Str,_),_,O,Ox) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showCanonTerm(apply(_,Op,Args,_),Dp,O,Ox) :-
  showCanonTerm(Op,Dp,O,O1),
  showCanonTerm(Args,Dp,O1,Ox).
showCanonTerm(dot(_,Rc,Fld,_),Dp,O,Ox) :-
  showCanonTerm(Rc,Dp,O,O1),
  appStr(".",O1,O2),
  appStr(Fld,O2,Ox).
showCanonTerm(enm(_,Nm,_),_,O,Ox) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,Ox).
showCanonTerm(cons(_,Nm,_),_,O,Ox) :-
  appStr("%",O,O1),
  appStr(Nm,O1,Ox).
showCanonTerm(case(_,Bound,Cases,_),Dp,O,Ox) :-
  showCanonTerm(Bound,Dp,O,O1),
  appStr(" in {",O1,O2),
  showRls("",Cases,Dp,O2,O3),
  appStr("}",O3,Ox).
showCanonTerm(theta(_,_Path,_,Defs,Others,Types,_),Dp,O,Ox) :-
  appStr("{",O,O2),
  Dp2 is Dp+2,
  appNwln(Dp2,O2,O3),
  showTypeDefs(Types,Dp2,O3,O4),
  showDefs(Defs,Dp2,O4,O5),
  showOthers(Others,Dp2,O5,O6),
  appStr("}",O6,Ox).
showCanonTerm(record(_,_Path,_,Defs,Others,Types,_),Dp,O,Ox) :-
  appStr("{.",O,O2),
  Dp2 is Dp+2,
  appNwln(Dp2,O2,O3),
  showTypeDefs(Types,Dp2,O3,O4),
  showDefs(Defs,Dp2,O4,O5),
  showOthers(Others,Dp2,O5,O6),
  appStr(".}",O6,Ox).
showCanonTerm(varRef(_,Inn),Dp,O,Ox) :-
  showCanonTerm(Inn,Dp,O,O1),
  appStr("!",O1,Ox).
showCanonTerm(assign(_,Vr,Vl),Dp,O,Ox) :-
  showCanonTerm(Vr,Dp,O,O1),
  appStr(":=",O1,O2),
  showCanonTerm(Vl,Dp,O2,Ox).
showCanonTerm(cell(_,Vr),Dp,O,Ox) :-
  appStr("!!",O,O1),
  showCanonTerm(Vr,Dp,O1,Ox).
showCanonTerm(letExp(_,Env,Ex),Dp,O,Ox) :-
  appStr("let ",O,O1),
  showCanonTerm(Env,Dp,O1,O2),
  appStr(" in ",O2,O3),
  showCanonTerm(Ex,Dp,O3,Ox).
showCanonTerm(lambda(_,Rle,_),Dp,O,Ox) :-
  appStr("(",O,O1),
  showRule("",Dp,Rle,O1,O2),
  appStr(")",O2,Ox).
showCanonTerm(tple(_,Els),Dp,O,Ox) :-
  appStr("(",O,O1),
  showTerms(Els,Dp,O1,O2),
  appStr(")",O2,Ox).
showCanonTerm(mtd(_,Nm,_),_,O,Ox) :-
  appStr("°",O,O1),
  appStr(Nm,O1,Ox).
showCanonTerm(over(_,V,_,Cons),Dp,O,Ox) :-
  showConstraints(Cons,O,O1),
  showCanonTerm(V,Dp,O1,Ox).
showCanonTerm(where(_,Ptn,Cond),Dp,O,Ox) :-
  showCanonTerm(Ptn,Dp,O,O1),
  showGuard(Cond,Dp,O1,Ox).
showCanonTerm(conj(_,L,R),Dp,O,Ox) :-
  showCanonTerm(L,Dp,O,O1),
  appStr(" && ",O1,O2),
  showCanonTerm(R,Dp,O2,Ox).
showCanonTerm(disj(_,Either,Or),Dp,O,Ox) :-
  appStr("(",O,O0),
  showCanonTerm(Either,Dp,O0,O1),
  appStr(" || ",O1,O2),
  showCanonTerm(Or,Dp,O2,O3),
  appStr(")",O3,Ox).
showCanonTerm(implies(_,L,R),Dp,O,Ox) :-
  showCanonTerm(L,Dp,O,O1),
  appStr(" *> ",O1,O2),
  showCanonTerm(R,Dp,O2,Ox).
showCanonTerm(cond(_,Test,Either,Or,_),Dp,O,Ox) :-
  appStr("(",O,O1),
  showCanonTerm(Test,Dp,O1,O2),
  appStr("?",O2,O3),
  showCanonTerm(Either,Dp,O3,O4),
  appStr(" | ",O4,O5),
  showCanonTerm(Or,Dp,O5,O6),
  appStr(")",O6,Ox).
showCanonTerm(match(_,P,E),Dp,O,Ox) :-
  showCanonTerm(P,Dp,O,O1),
  appStr(" .= ",O1,O2),
  showCanonTerm(E,Dp,O2,Ox).
showCanonTerm(search(_,P,S,M),Dp,O,Ox) :-
  showCanonTerm(P,Dp,O,O1),
  appStr(" in ",O1,O2),
  showCanonTerm(S,Dp,O2,O3),
  appStr(" using ",O3,O4),
  showCanonTerm(M,Dp,O4,Ox).
showCanonTerm(abstraction(_,Bound,Guard,G,_,_),Dp,O,Ox) :-
  appStr("{",O,O1),
  showCanonTerm(Bound,Dp,O1,O2),
  appStr("|",O2,O3),
  showCanonTerm(Guard,Dp,O3,O4),
  appStr("} using ",O4,O5),
  showCanonTerm(G,Dp,O5,Ox).
showCanonTerm(neg(_,R),Dp,O,Ox) :-
  appStr("\\+",O,O1),
  showCanonTerm(R,Dp,O1,Ox).
showCanonTerm(doTerm(_,Body,_,_,_),Dp,O,Ox) :-
  appStr("do ",O,O1),
  showCanonAction(Body,Dp,O1,Ox).

showTerms([],_,O,O).
showTerms([T|More],Dp,O,Ox) :-
  showCanonTerm(T,Dp,O,O1),
  showMoreTerms(More,Dp,O1,Ox).

showMoreTerms([],_,O,O).
showMoreTerms([T|More],Dp,O,Ox) :-
  appStr(", ",O,O1),
  showCanonTerm(T,Dp,O1,O2),
  showMoreTerms(More,Dp,O2,Ox).

showCanonAction(seqDo(Lc,A,B),Dp,O,Ox) :-
  appStr("{",O,O0),
  Dp2 is Dp+2,
  appNwln(Dp2,O0,O1),
  showCanonActions(seqDo(Lc,A,B),Dp2,O1,O2),
  appNwln(Dp,O2,O3),
  appStr("}",O3,Ox).
showCanonAction(delayDo(_,Actn,_,_,_),Dp,O,Ox) :-
  appStr("delay ",O,O1),
  showCanonAction(Actn,Dp,O1,Ox).
showCanonAction(bindDo(_,Ptn,Exp,_),Dp,O,Ox) :-
  showCanonTerm(Ptn,Dp,O,O1),
  appStr(" <- ",O1,O2),
  showCanonTerm(Exp,Dp,O2,Ox).
showCanonAction(varDo(_,Ptn,Exp),Dp,O,Ox) :-
  showCanonTerm(Ptn,Dp,O,O1),
  appStr(" = ",O1,O2),
  showCanonTerm(Exp,Dp,O2,Ox).
showCanonAction(ifThenDo(_,Tst,Th,noDo(_),_,_,_),Dp,O,Ox) :-
  appStr("if ",O,O1),
  Dp2 is Dp+2,
  showCanonTerm(Tst,Dp,O1,O2),
  appStr(" then ",O2,O3),
  showCanonAction(Th,Dp2,O3,Ox).
showCanonAction(ifThenDo(_,Tst,Th,El,_,_,_),Dp,O,Ox) :-
  appStr("if ",O,O1),
  Dp2 is Dp+2,
  showCanonTerm(Tst,Dp,O1,O2),
  appStr(" then ",O2,O3),
  showCanonAction(Th,Dp2,O3,O4),
  appStr(" else ",O4,O5),
  showCanonAction(El,Dp2,O5,Ox).
showCanonAction(whileDo(_,Tst,Bdy,_,_),Dp,O,Ox) :-
  appStr("while ",O,O1),
  Dp2 is Dp+2,
  showCanonTerm(Tst,Dp,O1,O2),
  appStr(" do ",O2,O3),
  showCanonAction(Bdy,Dp2,O3,Ox).
showCanonAction(forDo(_,Tst,Bdy,_,_),Dp,O,Ox) :-
  appStr("for ",O,O1),
  Dp2 is Dp+2,
  showCanonTerm(Tst,Dp2,O1,O2),
  appStr(" do ",O2,O3),
  appNwln(Dp2,O3,O4),
  showCanonAction(Bdy,Dp2,O4,Ox).
showCanonAction(tryCatchDo(_,Bdy,Hndlr,_,_,_),Dp,O,Ox) :-
  appStr("try ",O,O1),
  Dp2 is Dp+2,
  showCanonAction(Bdy,Dp2,O1,O2),
  appStr(" catch ",O2,O3),
  showCanonTerm(Hndlr,Dp2,O3,Ox).
showCanonAction(returnDo(_,Exp,_,_,_),Dp,O,Ox) :-
  appStr("valis ",O,O1),
  Dp2 is Dp+2,
  showCanonTerm(Exp,Dp2,O1,Ox).
showCanonAction(throwDo(_,Exp,_,_,_),Dp,O,Ox) :-
  appStr("throw ",O,O1),
  Dp2 is Dp+2,
  showCanonTerm(Exp,Dp2,O1,Ox).
showCanonAction(performDo(_,Exp,_,_,_),Dp,O,Ox) :-
  appStr("perform ",O,O1),
  Dp2 is Dp+2,
  showCanonTerm(Exp,Dp2,O1,Ox).
showCanonAction(simpleDo(_,Exp,_),Dp,O,Ox) :-
  showCanonTerm(Exp,Dp,O,Ox).
showCanonAction(noDo(_),_,O,Ox) :-
  appStr("nop",O,Ox).

showCanonActions(seqDo(_,A,B),Dp,O,Ox) :-
  showCanonAction(A,Dp,O,O1),
  appStr("; ",O1,O2),
  appNwln(Dp,O2,O3),
  showCanonActions(B,Dp,O3,Ox).
showCanonActions(A,Dp,O,Ox) :-
  showCanonAction(A,Dp,O,Ox).

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

showTypeDefs(L,Dp,O,Ox) :-
  listShow(L,canon:showTypeDef,misc:appNwln(Dp),O,Ox).

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

showImpl(imp(ImplName,FullName,Spec),O,Ox) :-
  appStr("implementation: ",O,O1),
  appStr(ImplName,O1,O2),
  appStr("/",O2,O3),
  appStr(FullName,O3,O4),
  appStr(" for ",O4,O5),
  showType(Spec,true,O5,O6),
  appStr("\n",O6,Ox).

showConstraints([],Ox,Ox).
showConstraints(Cons,O,Ox) :-
  listShow(Cons,types:showConstraint,misc:appStr(","),O,O1),
  appStr("|:",O1,Ox).

showDefs(L,Dp,O,Ox) :-
  listShow(L,canon:showDef(Dp),misc:appNwln(Dp),O,Ox).

showDef(Dp,funDef(Lc,Nm,ExtNm,Type,Cx,Eqns),O,Ox) :-
  appStr("function: ",O,O1),
  appIden(Nm,O1,O2),
  appStr("«",O2,O2a),
  appIden(ExtNm,O2a,O2b),
  appStr("» @ ",O2b,O3),
  showLocation(Lc,O3,O6),
  appNwln(Dp,O6,O7),
  showType(Type,true,O7,O8),
  showConstraints(Cx,O8,O9),
  appNwln(Dp,O9,O10),
  showRls(Nm,Eqns,Dp,O10,O11),
  appNwln(Dp,O11,Ox),!.
showDef(Dp,varDef(Lc,Nm,ExtNm,Cx,Tp,Value),O,Ox) :-
  appStr("var: ",O,O1),
  appIden(Nm,O1,O2),
  appStr("«",O2,O3),
  appIden(ExtNm,O3,O4),
  appStr("» : ",O4,O5),
  showType(Tp,true,O5,O6),
  appStr(" @ ",O6,O7),
  showLocation(Lc,O7,O8),
  showConstraints(Cx,O8,O9),
  appNwln(Dp,O9,O10),
  appIden(Nm,O10,O11),
  appStr(" = ",O11,O12),
  showCanonTerm(Value,Dp,O12,O13),
  appStr(".",O13,Ox).
showDef(Dp,cnsDef(Lc,Nm,V,Type),O,Ox) :-
  appStr("constructor: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(" @ ",O2,O3),
  showLocation(Lc,O3,O4),
  appNwln(Dp,O4,O5),
  showCanonTerm(V,Dp,O5,O6),
  appStr(":",O6,O7),
  showType(Type,true,O7,Ox).
showDef(Dp,typeDef(Lc,Nm,_Tp,Rl),O,Ox) :-
  appStr("type: ",O,O1),
  appStr(Nm,O1,O2),
  appStr(" @ ",O2,O5),
  showLocation(Lc,O5,O6),
  appNwln(Dp,O6,O7),
  showType(Rl,true,O7,Ox).
showDef(_,conDef(_,_,_),O,O).
showDef(_,implDef(_,_,_,_),O,O).

showRls(Nm,Rls,Dp,O,Ox) :-
  listShow(Rls,canon:showRule(Nm,Dp),misc:appMulti([misc:appStr("."),misc:appNwln(Dp)]),O,Ox).

showRule(Nm,Dp,equation(_,Args,Cond,Value),O,Ox) :-!,
  appStr(Nm,O,O1),
  showCanonTerm(Args,Dp,O1,O2),
  showGuard(Cond,Dp,O2,O5),
  appStr(" => ",O5,O6),
  showCanonTerm(Value,Dp,O6,Ox).

showGuard(enm(_,"true",_),_,O,O) :- !.
showGuard(C,Dp,O,Ox) :-
  appStr(" where ",O,O1),
  showCanonTerm(C,Dp,O1,Ox).

showOthers([],_,O,O).
showOthers([Stmt|Stmts],Dp,O,Ox) :-
  showStmt(Stmt,Dp,O,O2),
  appStr(".",O2,O3),
  appNwln(Dp,O3,O4),
  showOthers(Stmts,Dp,O4,Ox).

showStmt(assertion(_,Cond),Dp,O,Ox) :-
  appStr("  assert ",O,O1),
  showCanonTerm(Cond,Dp,O1,Ox).
showStmt(show(_,Vl),Dp,O,Ox) :-
  appStr("  show ",O,O1),
  showCanonTerm(Vl,Dp,O1,Ox).

ruleArity(equation(_,tple(_,A),_),Ar) :-
  length(A,Ar).

splitPtn(P,Px,Cond) :-
  locOfCanon(P,Lc),
  splitPttrn(P,Px,enm(Lc,"true",type("core.star*boolean")),Cond).

splitPttrn(apply(Lc,Op,Arg),apply(Lc,NOp,NArg),Cond,Cx) :-
  splitPttrn(Op,NOp,Cond,C0),
  splitPttrn(Arg,NArg,C0,Cx).
splitPttrn(tple(Lc,Els),tple(Lc,NEls),Cond,Cx) :-
  splitPttrns(Els,NEls,Cond,Cx).
splitPttrn(where(Lc,Ptn,Cond),P1,C,Cx) :-
  splitPttrn(Ptn,P1,C,C0),
  mergeGl(C0,Cond,Lc,Cx).
splitPttrn(P,P,C,C).

mergeGl(enm(_,"true",_),G,_,G) :-!.
mergeGl(G,enm(_,"true",_),_,G) :-!.
mergeGl(G1,G2,Lc,conj(Lc,G1,G2)).

splitPttrns([],[],C,C).
splitPttrns([P|Ps],[Px|Pxs],C,Cx) :-
  splitPttrn(P,Px,C,C0),
  splitPttrns(Ps,Pxs,C0,Cx).

anonVar(Lc,v(Lc,N,Tp),Tp) :-
  genstr("_",N),
  newTypeVar(N,Tp).

