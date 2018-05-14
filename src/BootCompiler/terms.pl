:- module(terms,[displayRules/1,displayEqns/2,substTerm/3,substTerms/3,
        genTplStruct/2,isTplStruct/1,isLiteral/1,isGround/1,mkTpl/2,isUnit/1,
        termHash/2,dispTerm/2,showTerm/4,locTerm/2]).

:- use_module(misc).
:- use_module(canon).
:- use_module(operators).
:- use_module(location).
:- use_module(types).

showRules(mdule(Pkg,Imports,Types,_,Defs,Contracts,Impls),O,Ox) :-
  appStr("Package: ",O,O0),
  showPkg(Pkg,O0,O1),
  appStr("\n",O1,O2),
  showImports(Imports,O2,O3),!,
  appStr("\nPackage export: ",O3,O3a),
  showType(Types,O3a,O3b),
  appStr("\nPackage contracts:\n",O3b,O4),!,
  showContracts(Contracts,O4,O5),!,
  appStr("\nPackage implementations:\n",O5,O5a),!,
  showImpls(Impls,O5a,O6),!,
  appStr("\nPackage definitions:\n",O6,O6a),!,
  showRuleSets(Defs,O6a,O7),!,
  appStr("\n",O7,Ox),!.

displayRules(Term) :- showRules(Term,Chrs,[]), string_chars(Res,Chrs), write(Res).

displayEqns(Nm,Term) :- showEqns(Term,Nm,Chrs,[]), string_chars(Res,Chrs), write(Res).


showRuleSets(L,O,Ox) :-
  listShow(L,terms:showRuleSet,misc:appNl,O,Ox).

showRuleSet(fnDef(_,Nm,Tp,Eqns),O,Ox) :-
  appStr("Function: ",O,O0),
  showTerm(Nm,0,O0,O1),
  appStr(":",O1,O2),
  showType(Tp,O2,O3),
  appStr("\n",O3,O4),
  showEqns(Eqns,Nm,O4,O5),
  appNl(O5,Ox).
showRuleSet(vrDef(_,Nm,Tp,Value),O,Ox) :-
  appStr("Global: ",O,O0),
  appStr(Nm,O0,O1),
  appStr(":",O1,O2),
  showType(Tp,O2,O3),
  appStr(" = ",O3,O4),
  showTerm(Value,0,O4,Ox).

showEqns(L,Nm,O,Ox) :-
  listShow(L,terms:showEqn(Nm),misc:appNl,O,Ox).

showEqn(Nm,eqn(_,Args,Value),O,Ox) :-
  showTerm(Nm,0,O,O1),
  showArgs(Args,0,O1,O2),
  appStr(" => ",O2,O3),
  showTerm(Value,0,O3,O4),
  appStr(".",O4,Ox).

showArgs(Args,Dp,O,Ox) :-
  showTerms(Args,"(",misc:appStr(","),")",Dp,O,Ox).

showTerms(Terms,Lft,Mid,Rgt,Dp,O,Ox) :-
  appStr(Lft,O,O1),
  listShow(Terms,terms:shwTerm(Dp),Mid,O1,O2),
  appStr(Rgt,O2,Ox).

shwTerm(Dp,T,O,Ox) :- showTerm(T,Dp,O,Ox).

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
  appStr(".",O1,O2),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,O2,Ox).
showTerm(ecll(_,Es,Args),Dp,O,Ox) :-
  appStr(Es,O,O1),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,O1,Ox).
showTerm(ctpl(Op,A),Dp,O,Ox) :-
  showTerm(Op,Dp,O,O1),
  Dp1 is Dp+2,
  showArgs(A,Dp1,O1,Ox).
showTerm(enum(Nm),_,O,Ox) :-
  appStr("'",O,O1),
  appStr(Nm,O1,O2),
  appStr("'",O2,Ox).
showTerm(lbl(Nm,Ar),_,O,Ox) :-
  appStr(Nm,O,O1),
  appStr("%",O1,O2),
  appInt(Ar,O2,Ox).
showTerm(whr(_,Ptn,Cond),Dp,O,Ox) :-
  showTerm(Ptn,Dp,O,O1),
  Dp1 is Dp+2,
  showTermGuard(Cond,Dp1,O1,Ox).
showTerm(varNames(_,Vars,Value),Dp,O,Ox) :-
  appStr("varDebug: [",O,O0),
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
  appStr("\\+",O,O1),
  showTerm(R,Dp,O1,Ox).
showTerm(error(_,Msg),_,O,Ox) :-
  appStr("error: ",O,O1),
  appQuoted(Msg,"\"",O1,Ox).

showTermGuard(enum("star.core#true"),_,Ox,Ox) :- !.
showTermGuard(T,Dp,O,Ox) :-
  appStr(" where ",O,O1),
  showTerm(T,Dp,O1,Ox).

showVarNames(Vrs,Dp,O,Ox) :-
  listShow(Vrs,terms:showVarBinding(Dp),misc:appStr(", "),O,Ox).

showVarBinding(Dp,(V,Vx),O,Ox) :-
  appStr(V,O,O1),
  appStr("/",O1,O2),
  showTerm(Vx,Dp,O2,Ox).

showCases(Cases,Dp,Dp1,O,Ox) :-
  listShow(Cases,terms:showCase(Dp1),terms:nxtCase(Dp),O,Ox).

nxtCase(Dp,O,Ox) :-
  appNl(O,O1),
  appIndx(Dp,O1,O2),
  appStr("| ",O2,Ox).

showCase(Dp,(Lbl,Val,_),O,Ox) :-
  showTerm(Lbl,Dp,O,O1),
  appStr(": ",O1,O2),
  showTerm(Val,Dp,O2,Ox).

dispTerm(T,Txt) :-
  showTerm(T,0,Chrs,[]),
  string_chars(Txt,Chrs).

substTerm(_,intgr(Ix),intgr(Ix)).
substTerm(Q,idnt(Nm),Trm) :- is_member((Nm,Trm),Q),!.
substTerm(_,idnt(Nm),idnt(Nm)).
substTerm(_,float(Dx),float(Dx)).
substTerm(_,strg(Sx),strg(Sx)).
substTerm(_,enum(Nm),enum(Nm)).
substTerm(_,lbl(Nm,Ar),lbl(Nm,Ar)).
substTerm(Q,cll(Lc,Op,Args),cll(Lc,NOp,NArgs)) :-
  substTerm(Q,Op,NOp),
  substTerms(Q,Args,NArgs).
substTerm(Q,ocall(Lc,Op,Args),ocall(Lc,NOp,NArgs)) :-
  substTerm(Q,Op,NOp),
  substTerms(Q,Args,NArgs).
substTerm(Q,ctpl(Op,Args),ctpl(NOp,NArgs)) :-
  substTerm(Q,Op,NOp),
  substTerms(Q,Args,NArgs).
substTerm(Q,ecll(Lc,Call,Args),ecll(Lc,Call,NArgs)) :-
  substTerms(Q,Args,NArgs).
substTerm(Q,whr(Lc,T,C),whr(Lc,NT,NC)) :-
  substTerm(Q,T,NT),
  substTerm(Q,C,NC).
substTerm(Q,varNames(Lc,V,T),varNames(Lc,NV,NT)) :-
  map(V,terms:substVN(Q),NV),
  substTerm(Q,T,NT).
substTerm(Q,case(Lc,T,C),case(Lc,NT,NC)) :-
  substTerm(Q,T,NT),
  map(C,terms:substCase(Q),NC).
substTerm(Q,seq(Lc,L,R),seq(Lc,NL,NR)) :-
  substTerm(Q,L,NL),
  substTerm(Q,R,NR).
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

genTplStruct(Cnt,lbl(Nm,Cnt)) :-
  swritef(Nm,"()%d",[Cnt]).

isTplStruct(lbl(Nm,Ar)) :- string_concat("()",A,Nm),number_string(Ar,A).

mkTpl(Els,ctpl(C,Els)) :-
  length(Els,Cnt),
  genTplStruct(Cnt,C).

isUnit(ctpl(lbl("()0",0),[])).

isLiteral(intgr(_)).
isLiteral(float(_)).
isLiteral(strg(_)).
isLiteral(enum(_)).
isLiteral(lbl(_,_)).

isGround(T) :- isLiteral(T),!.
isGround(ctpl(S,A)) :-
  isGround(S),
  forall(is_member(E,A), terms:isGround(E)).

termHash(intgr(Ix),Ix).
termHash(float(Dx),Ix) :- Ix is round(Dx).
termHash(strg(Sx),Ix) :- stringHash(0,Sx,Ix).
termHash(enum(Sx),Ix) :- termHash(lbl(Sx,0),Ix).
termHash(lbl(Nm,Ar),Hx) :-
  stringHash(0,Nm,Lx),
  Ix is Ar*37+Lx,
  hashSixtyFour(Ix,Hx).

locTerm(loc(Pk,Ln,Off,Str,Len),Tpl) :-
  mkTpl([strg(Pk),intgr(Ln),intgr(Off),intgr(Str),intgr(Len)],Tpl).
