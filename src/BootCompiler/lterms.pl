:- module(lterms,[displayRules/1,
		  dispRuleSet/1,substTerm/3,substTerms/3,rewriteTerm/3,
		  genTplStruct/2,isLiteral/1,isGround/1,isCnd/1,mkTpl/2,
		  isUnit/1,
		  termHash/2,dispTerm/1,showTerm/4,showArgs/4,locTerm/2,
		  idInTerm/2, isLTerm/1]).

:- use_module(misc).
:- use_module(canon).
:- use_module(operators).
:- use_module(location).
:- use_module(types).

isLTerm(idnt(_)) :- !.
isLTerm(voyd) :- !.
isLTerm(intgr(_)) :- !.
isLTerm(float(_)) :- !.
isLTerm(strg()) :- !.
isLTerm(cll(_,_,_)) :- !.
isLTerm(ocall(_,_,_)) :- !.
isLTerm(ecll(_,_,_)) :- !.
isLTerm(intrinsic(_,_,_)) :- !.
isLTerm(dte(_,_,_)) :- !.
isLTerm(ctpl(_,_)) :- !.
isLTerm(enum(_)) :- !.
isLTerm(lbl(_,_)) :- !.
isLTerm(whr(_,_,_)) :- !.
isLTerm(ltt(_,_,_,_)) :- !.
isLTerm(varNames(_,_,_)) :- !.
isLTerm(case(_,_,_,_)) :- !.
isLTerm(seq(_,_,_)) :- !.
isLTerm(cnj(_,_,_)) :- !.
isLTerm(cnd(_,_,_,_)) :- !.
isLTerm(dsj(_,_,_)) :- !.
isLTerm(mtch(_,_,_)) :- !.
isLTerm(ng(_,_)) :- !.
isLTerm(error(_,_)) :- !.
isLTerm(doAct(_,_)) :- !.

showRules(mdule(Pkg,Imports,Types,_,Defs,Contracts,Impls),O,Ox) :-
  appStr("Package: ",O,O0),
  showPkg(Pkg,O0,O1),
  appStr("\n",O1,O2),
  showImports(Imports,O2,O3),!,
  appStr("\nPackage export: ",O3,O3a),
  showType(Types,true,O3a,O3b),
  appStr("\nPackage contracts:\n",O3b,O4),!,
  showContracts(Contracts,O4,O5),!,
  appStr("\nPackage implementations:\n",O5,O5a),!,
  showImpls(Impls,O5a,O6),!,
  appStr("\nPackage definitions:\n",O6,O6a),!,
  showRuleSets(Defs,O6a,O7),!,
  appStr("\n",O7,Ox),!.

displayRules(Term) :- showRules(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

showRuleSets(L,O,Ox) :-
  listShow(L,lterms:showRuleSet,misc:appNl,O,Ox).

dispRuleSet(RS) :-
  showRuleSet(RS,Chrs,[]),
  string_chars(Res,Chrs), write(Res).

showRuleSet(fnDef(Lc,Nm,Tp,Args,Value),O,Ox) :-
  appStr("Function @ ",O,O1),
  showLocation(Lc,O1,O2),
  appNl(O2,O3),
  showTerm(Nm,0,O3,O4),
  appStr(":",O4,O5),
  showType(Tp,true,O5,O6),
  appNl(O6,O7),
  showTerm(Nm,0,O7,O8),
  showArgs(Args,0,O8,O9),
  appStr(" => ",O9,O10),
  showTerm(Value,0,O10,O11),
  appStr(".",O11,O12),
  appNl(O12,Ox).
showRuleSet(vrDef(Lc,Nm,Tp,Value),O,Ox) :-
  appStr("Global @ ",O,O1),
  showLocation(Lc,O1,O2),
  appNl(O2,O3),
  appStr(Nm,O3,O4),
  appStr(":",O4,O5),
  showType(Tp,true,O5,O6),
  appNl(O6,O7),
  appStr(Nm,O7,O8),
  appStr(" = ",O8,O9),
  showTerm(Value,0,O9,O10),
  appNl(O10,Ox).
showRuleSet(rcDef(Lc,Nm,Tp),O,Ox) :-
  appStr("Structure @ ",O,O1),
  showLocation(Lc,O1,O2),
  appNl(O2,O3),
  appIden(Nm,O3,O4),
  appStr(" ::= ",O4,O5),
  showType(Tp,true,O5,O6),
  appStr(".",O6,O7),
  appNl(O7,Ox).

showArgs(Args,Dp,O,Ox) :-
  showTerms(Args,"(",misc:appStr(","),")",Dp,O,Ox).

showTerms(Terms,Lft,Mid,Rgt,Dp,O,Ox) :-
  appStr(Lft,O,O1),
  listShow(Terms,lterms:shwTerm(Dp),Mid,O1,O2),
  appStr(Rgt,O2,Ox).

shwTerm(Dp,T,O,Ox) :- showTerm(T,Dp,O,Ox).

showTerm(voyd,_,O,Ox) :- appStr("void",O,Ox).
showTerm(idnt(Nm),_,O,Ox) :- appStr(Nm,O,Ox).
showTerm(intgr(Ix),_,O,Ox) :- appInt(Ix,O,Ox).
showTerm(float(Ix),_,O,Ox) :- appInt(Ix,O,Ox).
showTerm(strg(Str),_,O,Ox) :-
  appStr("""",O,O1),
  appStr(Str,O1,O2),
  appStr("""",O2,Ox).
showTerm(cll(_,Op,Args),Dp,O,Ox) :-
  showTerm(Op,Dp,O,O1),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,O1,Ox).
showTerm(ocall(_,Op,Args),Dp,O,Ox) :-
  showTerm(Op,Dp,O,O1),
  appStr(":",O1,O2),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,O2,Ox).
showTerm(ecll(_,Es,Args),Dp,O,Ox) :-
  appStr(Es,O,O1),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,O1,Ox).
showTerm(intrinsic(_,Op,Args),Dp,O,Ox) :-
  appStr("instr: ",O,O1),
  appStr(Op,O1,O2),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,O2,Ox).
showTerm(dte(_,Exp,Off),Dp,O,Ox) :-
  Dp1 is Dp+2,
  showTerm(Exp,Dp1,O,O1),
  appStr(".",O1,O2),
  showTerm(Off,Dp1,O2,Ox).
showTerm(ctpl(Op,A),Dp,O,Ox) :-
  showConOp(Op,Dp,O,O1),
  Dp1 is Dp+2,
  showArgs(A,Dp1,O1,Ox).
showTerm(enum(Nm),_,O,Ox) :-
  appStr(".",O,O1),
  appIden(Nm,O1,Ox).
showTerm(lbl(Nm,Ar),_,O,Ox) :-
  appStr(Nm,O,O1),
  appStr("%",O1,O2),
  appInt(Ar,O2,Ox).
showTerm(whr(_,Ptn,Cond),Dp,O,Ox) :-
  showTerm(Ptn,Dp,O,O1),
  Dp1 is Dp+2,
  showTermGuard(Cond,Dp1,O1,Ox).
showTerm(ltt(_,Vr,Bnd,Exp),Dp,O,Ox) :-
  appStr("let ",O,O1),
  Dp1 is Dp+2,
  showTerm(Vr,Dp1,O1,O2),
  appStr(" = ",O2,O3),
  showTerm(Bnd,Dp1,O3,O4),
  appStr(" in ",O4,O5),
  showTerm(Exp,Dp1,O5,Ox).
showTerm(varNames(_,Vars,Value),Dp,O,Ox) :-
  appStr("vars: [",O,O0),
  showVarNames(Vars,Dp,O0,O1),
  appStr("] -> ",O1,O2),
  showTerm(Value,Dp,O2,Ox).
showTerm(case(_,T,Cases,Deflt),Dp,O,Ox) :-
  appStr("case ",O,O1),
  showTerm(T,Dp,O1,O2),
  appStr(" in {\n",O2,O3),
  Dp1 is Dp+2,
  appIndx(Dp1,O3,O4),
  showCases(Cases,Dp,Dp1,O4,O5),
  appNwln(Dp,O5,O6),
  appStr("} else",O6,O7),
  appNwln(Dp1,O7,O8),
  showTerm(Deflt,Dp1,O8,Ox).
showTerm(seq(_,L,R),Dp,O,Ox) :-
  showTerm(L,Dp,O,O1),
  appStr(";",O1,O2),
  showTerm(R,Dp,O2,Ox).
showTerm(cnj(_,L,R),Dp,O,Ox) :-
  showTerm(L,Dp,O,O1),
  appStr(" && ",O1,O2),
  showTerm(R,Dp,O2,Ox).
showTerm(cnd(_,T,L,R),Dp,O,Ox) :-
  Dp1 is Dp+2,
  appStr("(",O,O0),
  showTerm(T,Dp,O0,O1),
  appStr(" ?",O1,O2),
  appNwln(Dp1,O2,O3),
  showTerm(L,Dp1,O3,O4),
  appNwln(Dp,O4,O5),
  appStr("| ",O5,O7),
  showTerm(R,Dp1,O7,O8),
  appNwln(Dp,O8,O9),
  appStr(")",O9,Ox).
showTerm(dsj(_,Either,Or),Dp,O,Ox) :-
  appStr("(",O,O0),
  showTerm(Either,Dp,O0,O1),
  appStr(" || ",O1,O2),
  showTerm(Or,Dp,O2,O3),
  appStr(")",O3,Ox).
showTerm(mtch(_,L,R),Dp,O,Ox) :-
  showTerm(L,Dp,O,O1),
  appStr(" .= ",O1,O2),
  showTerm(R,Dp,O2,Ox).
showTerm(ng(_,R),Dp,O,Ox) :-
  appStr("~",O,O1),
  showTerm(R,Dp,O1,Ox).
showTerm(error(_,Msg),_,O,Ox) :-
  appStr("error: ",O,O1),
  appQuoted(Msg,"\"",O1,Ox).
showTerm(doAct(_,Act),Dp,O,Ox) :-
  appStr("do {",O,O1),
  appNwln(Dp,O1,O2),
  Dp2 is Dp+2,
  showAction(Act,Dp2,O2,O3),
  appStr("}",O3,Ox).

showAction(seq(_,L,R),Dp,O,Ox) :-
  showAction(L,Dp,O,O1),
  appNwLn(Dp,O1,O2),
  showAction(R,Dp,O2,Ox).
showAction(seq(_,L,R),Dp,O,Ox) :-
  showAction(L,Dp,O,O1),
  appNwLn(Dp,O1,O2),
  showAction(R,Dp,O2,Ox).
showAction(varD(_,P,E),Dp,O,Ox) :-
  Dp2 is Dp+2,
  showTerm(P,Dp,O,O1),
  appStr(" = ",O1,O2),
  showTerm(E,Dp2,O2,Ox).
showAction(perf(_,E),Dp,O,Ox) :-
  appStr("perf ",O,O1),
  showTerm(E,Dp,O1,Ox).


showConOp(L,_,O,O) :-
  isTplStruct(L).
showConOp(L,Dp,O,Ox) :-
  showTerm(L,Dp,O,Ox).

showTermGuard(enum("star.core#true"),_,Ox,Ox) :- !.
showTermGuard(T,Dp,O,Ox) :-
  appStr(" where ",O,O1),
  showTerm(T,Dp,O1,Ox).

showVarNames(Vrs,Dp,O,Ox) :-
  listShow(Vrs,lterms:showVarBinding(Dp),misc:appStr(", "),O,Ox).

showVarBinding(Dp,(V,Vx),O,Ox) :-
  appStr(V,O,O1),
  appStr("/",O1,O2),
  showTerm(Vx,Dp,O2,Ox).

showCases(Cases,Dp,Dp1,O,Ox) :-
  listShow(Cases,lterms:showCase(Dp1),lterms:nxtCase(Dp),O,Ox).

nxtCase(Dp,O,Ox) :-
  appNl(O,O1),
  appIndx(Dp,O1,O2),
  appStr("| ",O2,Ox).

showCase(Dp,(Lbl,Val,_),O,Ox) :-
  showTerm(Lbl,Dp,O,O1),
  appStr(": ",O1,O2),
  showTerm(Val,Dp,O2,Ox).

dispTerm(T) :-
  showTerm(T,0,Chrs,[]),
  string_chars(Txt,Chrs), writeln(Txt).

substTerm(Q,In,Out) :-
  rewriteTerm(lterms:applyQ(Q),In,Out),!.

applyQ(Q,idnt(Nm),Trm) :- is_member((Nm,Trm),Q),!.

substTerms(Q,Els,NEls):-
  map(Els,lterms:substTerm(Q),NEls).

rewriteTerm(QTst,T,T1) :-
  call(QTst,T,T1),!.
rewriteTerm(_,voyd,voyd).
rewriteTerm(_,intgr(Ix),intgr(Ix)).
rewriteTerm(_,idnt(Nm),idnt(Nm)).
rewriteTerm(_,float(Dx),float(Dx)).
rewriteTerm(_,strg(Sx),strg(Sx)).
rewriteTerm(_,enum(Nm),enum(Nm)).
rewriteTerm(_,lbl(Nm,Ar),lbl(Nm,Ar)).
rewriteTerm(QTest,ltt(Lc,V,Val,Exp),ltt(Lc,V,Val1,Exp1)) :-
  rewriteTerm(lterms:checkV(V,QTest),Val,Val1),
  rewriteTerm(lterms:checkV(V,QTest),Exp,Exp1).
rewriteTerm(QTest,cll(Lc,Op,Args),cll(Lc,NOp,NArgs)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,ocall(Lc,Op,Args),ocall(Lc,NOp,NArgs)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,dte(Lc,Op,Off),dte(Lc,NOp,NOff)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerm(QTest,Off,NOff).
rewriteTerm(QTest,ctpl(Op,Args),ctpl(NOp,NArgs)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,intrinsic(Lc,Op,Args),intrinsic(Lc,Op,NArgs)) :-
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,ecll(Lc,Call,Args),ecll(Lc,Call,NArgs)) :-
  rewriteTerms(QTest,Args,NArgs).
rewriteTerm(QTest,whr(Lc,T,C),whr(Lc,NT,NC)) :-
  rewriteTerm(QTest,T,NT),
  rewriteTerm(QTest,C,NC).
rewriteTerm(QTest,varNames(Lc,V,T),varNames(Lc,NV,NT)) :-
  map(V,lterms:rewriteVN(QTest),NV),
  rewriteTerm(QTest,T,NT).
rewriteTerm(QTest,case(Lc,T,C,D),case(Lc,NT,NC,ND)) :-
  rewriteTerm(QTest,T,NT),
  map(C,lterms:rewriteCase(QTest),NC),
  rewriteTerm(QTest,D,ND).
rewriteTerm(QTest,seq(Lc,L,R),seq(Lc,NL,NR)) :-
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,cnj(Lc,L,R),cnj(Lc,NL,NR)) :-
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,dsj(Lc,L,R),dsj(Lc,NL,NR)) :-
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,cnd(Lc,T,L,R),cnd(Lc,NT,NL,NR)) :-
  rewriteTerm(QTest,T,NT),
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,mtch(Lc,L,R),mtch(Lc,NL,NR)) :-
  rewriteTerm(QTest,L,NL),
  rewriteTerm(QTest,R,NR).
rewriteTerm(QTest,ng(Lc,R),ng(Lc,NR)) :-
  rewriteTerm(QTest,R,NR).
rewriteTerm(_,error(Lc,Msg),error(Lc,Msg)) :-!.

rewriteTerms(QTest,Els,NEls):-
  map(Els,lterms:rewriteTerm(QTest),NEls).

rewriteVN(QTest,(T,E),(T,NE)) :-
  rewriteTerm(QTest,E,NE).

rewriteCase(QTest,(T,E,Lbl),(NT,NE,Lbl)) :-
  rewriteTerm(QTest,T,NT),
  rewriteTerm(QTest,E,NE).

checkV(Vr,Other,T,T1) :-
  T\=Vr,
  call(Other,T,T1).

genTplStruct(Cnt,lbl(Nm,Cnt)) :-
  swritef(Nm,"()%d",[Cnt]).

isTplStruct(lbl(Nm,Ar)) :- string_concat("()",A,Nm),number_string(Ar,A).

mkTpl(Els,ctpl(C,Els)) :-
  length(Els,Cnt),
  genTplStruct(Cnt,C).

isUnit(ctpl(lbl("()0",0),[])).

isLiteral(voyd).
isLiteral(intgr(_)).
isLiteral(float(_)).
isLiteral(strg(_)).
isLiteral(enum(_)).
isLiteral(lbl(_,_)).

isGround(T) :- isLiteral(T),!.
isGround(ctpl(S,A)) :-
  isGround(S),
  check_implies(is_member(E,A), lterms:isGround(E)).

termHash(voyd,0).
termHash(intgr(Ix),Ix).
termHash(float(Dx),Ix) :- Ix is round(Dx).
termHash(strg(Sx),Ix) :- stringHash(0,Sx,Ix).
termHash(enum(Sx),Ix) :- termHash(lbl(Sx,0),Ix).
termHash(lbl(Nm,Ar),Hx) :-
  stringHash(0,Nm,Lx),
  Ix is Ar*37+Lx,
  hashSixtyFour(Ix,Hx).

locTerm(loc(Pk,Ln,Col,Pos,Len),Tpl) :-
  mkTpl([strg(Pk),intgr(Ln),intgr(Col),intgr(Pos),intgr(Len)],Tpl).

idInTerm(idnt(Nm),Term) :-
  inTerm(Term,Nm),!.

inTerm(idnt(Nm),Nm).
inTerm(cll(_,_,Args),Nm) :-
  is_member(Arg,Args),
  inTerm(Arg,Nm).
inTerm(ocall(_,Op,_),Nm) :-
  inTerm(Op,Nm).
inTerm(ocall(_,_Op,Args),Nm) :-
  is_member(Arg,Args), inTerm(Arg,Nm),!.
inTerm(dte(_,Op,Off),Nm) :-
  inTerm(Op,Nm); inTerm(Off,Nm).
inTerm(ctpl(_,Args),Nm) :-
  is_member(Arg,Args), inTerm(Arg,Nm),!.
inTerm(ecll(_,_,Args),Nm) :-
  is_member(Arg,Args), inTerm(Arg,Nm),!.
inTerm(whr(_,T,_),Nm) :-
  inTerm(T,Nm),!.
inTerm(whr(_,_,C),Nm) :-
  inTerm(C,Nm),!.
inTerm(varNames(_,V,_),Nm) :-
  is_member((Nm,_),V),!.
inTerm(varNames(_,_,T),Nm) :-
  inTerm(T,Nm),!.
inTerm(case(_,T,_C),Nm) :-
  inTerm(T,Nm),!.
inTerm(case(_,_T,C),Nm) :-
  is_member((P,V),C), (inTerm(P,Nm);inTerm(V,Nm)),!.
inTerm(seq(_,L,R),Nm) :-
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(cnj(_,L,R),Nm) :-
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(dsj(_,L,R),Nm) :-
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(cnd(_,T,L,R),Nm) :-
  inTerm(T,Nm) ; inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(mtch(_,L,R),Nm) :-
  inTerm(L,Nm) ; inTerm(R,Nm).
inTerm(ng(_,R),Nm) :-
  inTerm(R,Nm).

isCnd(cnj(_,_,_)).
isCnd(dsj(_,_,_)).
isCnd(mtch(_,_,_)).
isCnd(ng(_,_)).


