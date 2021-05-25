:- module(lterms,[ssTransformed/2,
		  dispRuleSet/1,dispProg/1,
		  substTerm/3,substGoal/3,substTerms/3,rewriteTerm/3,
		  genTplStruct/2,isLiteral/1,isGround/1,isCnd/1,mkTpl/2,
		  isTplLbl/2,mkCons/3,
		  isUnit/1,
		  termHash/2,
		  ssTrm/3,dispTerm/1,showTerm/4,locTerm/2,
		  idInTerm/2, isLTerm/1,
		  validLProg/1]).

:- use_module(display).
:- use_module(misc).
:- use_module(canon).
:- use_module(operators).
:- use_module(location).
:- use_module(types).
:- use_module(escapes).
:- use_module(intrinsics).
:- use_module(errors).

isLTerm(idnt(_)) :- !.
isLTerm(voyd) :- !.
isLTerm(intgr(_)) :- !.
isLTerm(float(_)) :- !.
isLTerm(strg()) :- !.
isLTerm(cll(_,_,_)) :- !.
isLTerm(ocall(_,_,_)) :- !.
isLTerm(ecll(_,_,_)) :- !.
isLTerm(intrinsic(_,_,_)) :- !.
isLTerm(nth(_,_,_)) :- !.
isLTerm(setix(_,_,_,_)) :- !.
isLTerm(ctpl(_,_)) :- !.
isLTerm(enum(_)) :- !.
isLTerm(lbl(_,_)) :- !.
isLTerm(whr(_,_,_)) :- !.
isLTerm(ltt(_,_,_,_)) :- !.
isLTerm(varNames(_,_,_)) :- !.
isLTerm(case(_,_,_,_)) :- !.
isLTerm(unpack(_,_,_)) :- !.
isLTerm(seq(_,_,_)) :- !.
isLTerm(cnj(_,_,_)) :- !.
isLTerm(cnd(_,_,_,_)) :- !.
isLTerm(dsj(_,_,_)) :- !.
isLTerm(mtch(_,_,_)) :- !.
isLTerm(ng(_,_)) :- !.
isLTerm(error(_,_)) :- !.
isLTerm(doAct(_,_)) :- !.

ssTransformed(mdule(Pkg,_Imports,Tp,_,_,Defs,_Contracts,Impls),
	      sq([ss("Package "),canon:ssPkg(Pkg),ss(" type "),TT,
		  nl(0),iv(nl(0),Rs),
		  nl(0),ss("Implementations\n"),iv(nl(0),Is)])):-
  ssType(Tp,true,0,TT),
  map(Defs,lterms:ssRuleSet,Rs),
  map(Impls,canon:ssImpl(0),Is).

ssRuleSet(fnDef(_Lc,Nm,_Tp,Args,Value),sq([ss("Fn: "),NN,lp,AA,rp,ss(" => "),VV])) :-
  ssTrm(Nm,0,NN),
  showArgs(Args,0,AA),
  ssTrm(Value,0,VV).
ssRuleSet(glbDef(_Lc,Nm,_Tp,Value),sq([ss("Gl: "),id(Nm),ss(" = "),VV])) :-
  ssTrm(Value,0,VV).
ssRuleSet(tpDef(_Lc,Tp,_Rl,IxMap),
	  sq([ss("Tp: "),TT,ss(" : "),MM])) :-
  ssType(Tp,false,0,TT),
  ssConsMap(IxMap,MM).

dispProg(M) :-
  ssTransformed(M,S),
  validSS(S),!,
  displayln(S).

dispRuleSet(RS) :-
  display:displayln(lterms:ssRuleSet(RS)).

ssConsMap(Els,iv(ss(", "),EE)) :-
  map(Els,lterms:ssConsEntry,EE).

ssConsEntry((Lb,Ix),sq([LL,ss(":"),ix(Ix)])) :-
  ssTrm(Lb,0,LL).
  
showArgs(Args,Dp,iv(ss(","),AA)) :-
  map(Args,lterms:swTrm(Dp),AA).

swTrm(Dp,T,S) :- ssTrm(T,Dp,S).

showTerm(Trm,Dp,O,Ox) :-
  ss_to_chrs(lterms:ssTrm(Trm,Dp),O,Ox).

ssTrm(voyd,_,ss("void")).
ssTrm(idnt(Nm),_,id(Nm)).
ssTrm(intgr(Ix),_,ix(Ix)).
ssTrm(float(Dx),_,fx(Dx)).
ssTrm(strg(Str),_,sq([ss(""""),ss(Str),ss("""")])).
ssTrm(cll(_,Op,Args),Dp,sq([OO,lp,AA,rp])) :-
  ssTrm(Op,Dp,OO),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ocall(_,Op,Args),Dp,sq([OO,ss("°"),lp,AA,rp])) :-
  ssTrm(Op,Dp,OO),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ecll(_,Es,Args),Dp,sq([ss("ε"),ss(Es),ss("("),AA,ss(")")])) :-
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(intrinsic(_,Op,Args),Dp,sq([id(OpNm),ss("<"),AA,ss(">")])) :-
  atom_string(Op,OpNm),
  Dp1 is Dp+2,
  showArgs(Args,Dp1,AA).
ssTrm(ctpl(Op,A),Dp,sq([ss("."),OO,lp,AA,rp])) :-
  ssCOnOp(Op,OO),
  Dp1 is Dp+2,
  showArgs(A,Dp1,AA).
ssTrm(enum(Nm),_,sq([ss("."),id(Nm)])).
ssTrm(nth(_,Rc,Off),Dp,sq([OO,ss("."),ix(Off)])) :-
  ssTrm(Rc,Dp,OO).
ssTrm(setix(_,Rc,Off,Vl),Dp,sq([OO,ss("."),ix(Off),ss(":="),VV])) :-
  ssTrm(Rc,Dp,OO),
  ssTrm(Vl,Dp,VV).
ssTrm(lbl(Nm,Ar),_,sq([id(Nm),ss("/"),ix(Ar)])).
ssTrm(whr(_,Ptn,Cond),Dp,sq([PP,ss(" whr "),CC])) :-
  ssTrm(Ptn,Dp,PP),
  Dp1 is Dp+2,
  ssTrm(Cond,Dp1,CC).
ssTrm(ltt(_,Vr,Bnd,Exp),Dp,sq([ss("let "),VV,ss("="),BB,ss(" in "),EE])) :-
  Dp1 is Dp+2,
  ssTrm(Vr,Dp1,VV),
  ssTrm(Bnd,Dp1,BB),
  ssTrm(Exp,Dp1,EE).
ssTrm(varNames(_,Vars,Value),Dp,sq([ss("vars:"),VV,ss("->"),EE])) :-
  ssVarNames(Vars,Dp,VV),
  ssTrm(Value,Dp,EE).
ssTrm(case(_,G,Cases,Deflt),Dp,
      sq([ss("case "),GG,ss("in"),lb,CC,rb,ss(" else "),DD])) :-
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,CC),
  ssTrm(Deflt,Dp,DD).
ssTrm(unpack(_,G,Cases),Dp,
      sq([ss("unpack "),GG,ss(" in "),CC])) :-
  ssTrm(G,Dp,GG),
  ssCases(Cases,Dp,CC).
ssTrm(seq(_,L,R),Dp,sq([LL,ss(";"),RR])) :-
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(cnj(_,L,R),Dp,sq([LL,ss("&&"),RR])) :-
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(dsj(_,L,R),Dp,sq([lp,LL,ss("||"),RR,rp])) :-
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(cnd(_,T,L,R),Dp,sq([lp,TT,ss(" ? "),LL,ss("||"),RR,rp])) :-
  ssTrm(T,Dp,TT),
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(mtch(_,L,R),Dp,sq([lp,LL,ss(".="),RR,rp])) :-
  ssTrm(L,Dp,LL),
  ssTrm(R,Dp,RR).
ssTrm(ng(_,R),Dp,sq([lp,ss("~"),RR,rp])) :-
  ssTrm(R,Dp,RR).
ssTrm(error(Lc,M),Dp,sq([lp,ss("error "),MM,rp,ss("@"),LL])) :-
  ssTrm(M,Dp,MM),
  ssLoc(Lc,LL).
ssTrm(doAct(_,Act),Dp,sq([ss("do "),AA])) :-
  ssAct(Act,Dp,AA).

ssAct(seq(_,L,R),Dp,sq([LL,ss(";"),RR])) :-
  ssAct(L,Dp,LL),
  ssAct(R,Dp,RR).
ssAct(varD(_,P,E),Dp,sq([PP,ss(" = "),EE])) :-
  ssTrm(P,Dp,PP),
  ssTrm(E,Dp,EE).
ssAct(perf(_,E),Dp,sq([ss("perform "),EE])) :-
  ssTrm(E,Dp,EE).

ssCases(Cases,Dp,sq([lb,nl(Dp2),iv(nl(Dp2),CC),nl(Dp),rb])) :-
  Dp2 is Dp+2,
  map(Cases,lterms:ssCase(Dp2),CC).

ssCase(Dp,(Ptn,Val,_),sq([PP,ss("=>"),VV])) :-
  ssTrm(Ptn,Dp,PP),
  ssTrm(Val,Dp,VV).

ssCOnOp(lbl(L,Ar),sq([])) :-
  isTplLbl(L,Ar).
ssCOnOp(lbl(Nm,_),id(Nm)).

ssTrmGuard(none,_,sq([])) :- !.
ssTrmGuard(some(C),Dp,sq([ss("where "),CC])) :-
  ssTrm(C,Dp,CC).

ssVarNames(Vrs,Dp,iv(ss(","),VV)) :-
  map(Vrs,lterms:ssVarBinding(Dp),VV).

ssVarBinding(Dp,(V,Vx),ss([id(V),ss("/"),VV])) :-
  ssTrm(Vx,Dp,VV).

dispTerm(T) :-
  display:display(lterms:ssTrm(T,0)).

substTerm(Q,In,Out) :-
  rewriteTerm(lterms:applyQ(Q),In,Out),!.

substGoal(_,none,none) :-!.
substGoal(Q,some(In),some(Out)) :-
  substTerm(Q,In,Out).

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
rewriteTerm(QTest,nth(Lc,Op,Off),nth(Lc,NOp,Off)) :-
  rewriteTerm(QTest,Op,NOp).
rewriteTerm(QTest,setix(Lc,Op,Off,Vl),setix(Lc,NOp,Off,NVl)) :-
  rewriteTerm(QTest,Op,NOp),
  rewriteTerm(QTest,Vl,NVl).
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
rewriteTerm(QTest,unpack(Lc,T,C),unpack(Lc,NT,NC)) :-
  rewriteTerm(QTest,T,NT),
  map(C,lterms:rewriteCase(QTest),NC).
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
rewriteTerm(_,error(Lc,M),error(Lc,strg(M))) :-!.

rewriteTerms(QTest,Els,NEls):-
  map(Els,lterms:rewriteTerm(QTest),NEls).

rewriteGoal(_,none,none).
rewriteGoal(QTest,some(T),some(NT)) :-
  rewriteTerm(QTest,T,NT).

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

isTplLbl(Nm,Ar) :- string_concat("()",A,Nm),number_string(Ar,A).

mkTpl(Els,ctpl(C,Els)) :-
  length(Els,Cnt),
  genTplStruct(Cnt,C).

mkCons(Nm,Els,ctpl(lbl(Nm,Arity),Els)) :-
  length(Els,Arity).

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
inTerm(nth(_,Op,_),Nm) :-
  inTerm(Op,Nm).
inTerm(setix(_,Op,_,_),Nm) :-
  inTerm(Op,Nm).
inTerm(setix(_,_,_,Vl),Nm) :-
  inTerm(Vl,Nm).
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
inTerm(unpack(_,T,_C),Nm) :-
  inTerm(T,Nm),!.
inTerm(unpack(_,_T,C),Nm) :-
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
inTerm(ltt(_,_,B,_E),Nm) :-
  inTerm(B,Nm).
inTerm(ltt(_,_,_B,E),Nm) :-
  inTerm(E,Nm).


isCnd(cnj(_,_,_)).
isCnd(dsj(_,_,_)).
isCnd(mtch(_,_,_)).
isCnd(ng(_,_)).

validLProg(mdule(_Pkg,_Imports,_Tp,_Face,_,Defs,_Contracts,_Impls)) :-
  declareNms(Defs,[],Dct),
  validDfs(Defs,Dct).

validDfs([],_).
validDfs([D|Dfs],Dct) :-
  validDf(D,Dct),!,
  validDfs(Dfs,Dct).

validDf(fnDef(Lc,_,_Tp,Args,Value),Dct) :-!,
  declareArgs(Args,Dct,D0),
  validTerm(Value,Lc,D0),!.
validDf(glbDef(Lc,_Nm,_Tp,Value),Dct) :-
  validTerm(Value,Lc,Dct).
validDf(tpDef(_Lc,_Tp,_Rl,_IxMap),_).

declareNms(Defs,Dct,Dx) :-
  rfold(Defs,lterms:declareDef,Dct,Dx).

declareDef(fnDef(_Lc,Nm,_Tp,_Args,_Value),Dct,Dx) :-!,
  add_mem(Nm,Dct,Dx).
declareDef(glbDef(_Lc,Nm,_Tp,_Value),Dct,Dx) :-
  add_mem(Nm,Dct,Dx).
declareDef(tpDef(_Lc,_Tp,_Rl,_IxMap),Dx,Dx).
       
declareArgs(Args,Dct,Dx) :-
  rfold(Args,lterms:declareArg,Dct,Dx).

declareArg(idnt(Nm),D,Dx) :-
  add_mem(Nm,D,Dx).

validTerm(idnt(Nm),Lc,D) :-
  (is_member(Nm,D) -> true ; 
   reportError("(validate) Variable %s not in scope %s",[Nm,D],Lc),
   abort).
validTerm(voyd,_,_).
validTerm(intgr(_),_,_).
validTerm(float(_),_,_).
validTerm(strg(_),_,_).
validTerm(lbl(_,_),_,_).
validTerm(cll(Lc,lbl(_,_),Args),_,D) :-
  check_implies(is_member(A,Args),lterms:validTerm(A,Lc,D)).
validTerm(ocall(Lc,Op,Args),_,D) :-
  validTerm(Op,Lc,D),
  check_implies(is_member(A,Args),lterms:validTerm(A,Lc,D)).
validTerm(ecll(Lc,Es,Args),_,D) :-
  isEscape(Es),
  check_implies(is_member(A,Args),lterms:validTerm(A,Lc,D)).
validTerm(intrinsic(Lc,Is,Args),_,D) :-
  isIntrinsic(_,_,Is),
  check_implies(is_member(A,Args),lterms:validTerm(A,Lc,D)).
validTerm(ctpl(lbl(_,_),Args),Lc,D) :-
  check_implies(is_member(A,Args),lterms:validTerm(A,Lc,D)).
validTerm(enum(_),_,_).
validTerm(nth(Lc,Rc,Off),_,D) :-
  integer(Off),
  validTerm(Rc,Lc,D).
validTerm(setix(Lc,Rc,Off,Vl),_,D) :-
  integer(Off),
  validTerm(Rc,Lc,D),
  validTerm(Vl,Lc,D).
validTerm(whr(Lc,Exp,Cond),_,D) :-
  glVars(Cond,D,D1),
  validTerm(Exp,Lc,D1),
  validTerm(Cond,Lc,D1).
validTerm(ltt(Lc,Vr,Bnd,Exp),_,D) :-
  validTerm(Bnd,Lc,D),
  ptnVars(Vr,D,D1),
  validTerm(Vr,Lc,D1),
  validTerm(Exp,Lc,D1).
validTerm(varNames(Lc,Vars,Value),_,D) :-
  rfold(Vars,lterms:validVr(Lc),D,D1),
  validTerm(Value,Lc,D1).
validTerm(case(Lc,G,Cases,Deflt),_,D) :-
  validTerm(G,Lc,D),
  validCases(Cases,D),
  validTerm(Deflt,Lc,D).
validTerm(unpack(Lc,G,Cases),_,D) :-
  validTerm(G,Lc,D),
  validCases(Cases,D).
validTerm(seq(_,L,R),Lc,D) :-
  validTerm(L,Lc,D),
  validTerm(R,Lc,D).
validTerm(cnj(_,L,R),Lc,D) :-
  validTerm(L,Lc,D),
  validTerm(R,Lc,D).
validTerm(dsj(_,L,R),Lc,D) :-
  validTerm(L,Lc,D),
  validTerm(R,Lc,D).
validTerm(cnd(Lc,T,L,R),_,D) :-
  glVars(T,D,D0),
  validTerm(T,Lc,D0),
  validTerm(L,Lc,D0),
  validTerm(R,Lc,D).
validTerm(mtch(Lc,L,R),Lc,D) :-
  validTerm(L,Lc,D),
  validTerm(R,Lc,D).
validTerm(ng(Lc,R),_,D) :-
  validTerm(R,Lc,D).
validTerm(error(Lc,R),_,D) :-
  validTerm(R,Lc,D).
validTerm(doAct(Lc,Act),_,D) :-
  validAct(Act,Lc,D).
validTerm(T,Lc,D) :-
  reportError("(internal) Invalid term %s in scope %s",[ltrm(T),D],Lc),
  abort.


validCases(Cases,D) :-
  check_implies(is_member((Ptn,Val,Lc),Cases),lterms:validCase(Lc,Ptn,Val,D)).

validCase(Lc,Ptn,Val,D) :-
  ptnVars(Ptn,D,D1),
  validTerm(Ptn,Lc,D1),
  validTerm(Val,Lc,D1).

validAct(seq(Lc,varD(_,V,B),R),_,D) :-
  ptnVars(V,D,D0),
  validTerm(V,Lc,D0),
  validTerm(B,Lc,D),
  validAct(R,Lc,D0).
validAct(seq(Lc,L,R),_,D) :-
  validAct(L,Lc,D),
  validAct(R,Lc,D).
showAction(varD(Lc,P,E),_,D) :-
  ptnVars(P,D,D0),
  validTerm(P,Lc,D0),
  validTerm(E,Lc,D).

ptnVars(idnt(Nm),D,Dx) :-
  add_mem(Nm,D,Dx).
ptnVars(voyd,Dx,Dx).
ptnVars(intgr(_),Dx,Dx).
ptnVars(float(_),Dx,Dx).
ptnVars(strg(_),Dx,Dx).
ptnVars(lbl(Dx,Dx),Dx,Dx).
ptnVars(cll(_,_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(ocall(_,_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(ecll(_,_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(intrinsic(_,_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(ctpl(_,Args),D,Dx) :-
  rfold(Args,lterms:ptnVars,D,Dx).
ptnVars(enum(_),Dx,Dx).
ptnVars(nth(_,Rc,_),D,Dx) :-
  ptnVars(Rc,D,Dx).
ptnVars(whr(_,Ptn,Cond),D,Dx) :-
  ptnVars(Ptn,D,D0),
  glVars(Cond,D0,Dx).

glVars(cnj(_,L,R),D,Dx) :-
  glVars(L,D,D0),
  glVars(R,D0,Dx).
glVars(dsj(_,L,R),D,Dx) :-
  glVars(L,D,D0),
  glVars(R,D0,Dx).
glVars(cnd(_,T,L,R),D,Dx) :-
  glVars(T,[],D0),
  glVars(L,D0,D1),
  glVars(R,D,D2),
  intersect(D1,D2,Di),
  merge(Di,D,Dx).
glVars(mtch(_,L,_),D,Dx) :-
  ptnVars(L,D,Dx).
glVars(_,Dx,Dx).

