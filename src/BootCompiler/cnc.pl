:- module(cnc,[genAbstraction/3]).

:- use_module(wff).
:- use_module(types).
:- use_module(canon).
:- use_module(unify).
:- use_module(abstract).
:- use_module(terms).
:- use_module(freevars).
:- use_module(misc).

analyseCondition(search(_,Ptn,Src,_),Df,Dfx,Rq,Rqx,Cand) :-
  analysePtn(Ptn,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Src,Df1,Dfx,Rq1,Rqx,Cand).

/*
 * { Exp | Cond }
 *
 * becomes
 * generator((X)=>Exp,nil)
 * where generator is a function that implements the Cond
 */
genAbstraction(abstraction(Lc,El,Cond,Gen,Tp),Path,Exp) :-
  IterTp = tpExp(tpFun("star.iterable*iterState",1),Tp),
  InitState = enm(Lc,"noneFound",IterTp),
  genCondition(Cond,Path,cnc:genEl(Lc,El,Gen,IterTp),abort(Lc),InitState,Seq),
  typeOfCanon(El,ElTp),
  Exp = apply(Lc,v(Lc,"unwrapIter",funType(tupleType([ElTp,IterTp]),IterTp)),tple(Lc,[Seq]),Tp).

/*
 * Ptn in Src
 * becomes
 * let{
 *  sF(Ptn,St) => AddEl(X,St).
 *  sF(_,St) default => St.
 * } in _iterate(Src,sF,Initial)
 *
 * where AddEl, InitState are parameters to the conversion
 */
genCondition(search(Lc,Ptn,Src,Iterator),Path,Succ,_Fail,Initial,Exp) :-
  genstr("f",Fn),
  genNme(Lc,PtnTp,"_",Anon),
  genNme(Lc,StTp,"_st",St),
  genstr("Γ",ThNm),
  thetaName(Path,ThNm,ThPath),
  packageVarName(ThPath,Fn,LclName),
  call(Succ,St,AddToFront),
  FF=funDef(Lc,Fn,LclName,FnTp,[],[
    equation(Lc,tple(Lc,[Ptn,St]),enm(Lc,"true",type("star.core*boolean")),AddToFront),
    equation(Lc,tple(Lc,[Anon,St]),enm(Lc,"true",type("star.core*boolean")),St)
  ]),
  Let = letExp(Lc,theta(Lc,ThNm,[FF],[],[],faceType([],[])),v(Lc,Fn,FnTp)),
  Exp = apply(Lc,Iterator,tple(Lc,[Src,Let,Initial]),StTp),
  typeOfCanon(Src,SrcTp),
  StTp = tpExp(tpFun("star.iterable*iterState",1),SrcTp),
  typeOfCanon(Ptn,PtnTp),
  FnTp = funType(tupleType([PtnTp,StTp]),StTp).
genCondition(conj(_Lc,A,B),Path,Succ,Fail,Initial,Exp) :-
  genCondition(A,Path,cnc:genCondition(B,Path,Succ,Fail),Fail,Initial,Exp).
genCondition(disj(_,A,B),Path,Succ,Fail,Initial,Exp) :-
  genCondition(A,Path,Succ,Fail,Initial,E1),
  genCondition(B,Path,Succ,Fail,E1,Exp).

genNme(Lc,Tp,Pr,v(Lc,Nm,Tp)) :-
  genstr(Pr,Nm).

genEl(Lc,El,Gen,StTp,St,apply(Lc,Gen,tple(Lc,[El,St]),StTp)).


analyseExp(v(Lc,Nm,Tp),Dfx,Dfx,Rq,Rqx,Cand) :-
  is_member(v(_,Nm,_),Cand) -> addVar(v(Lc,Nm,Tp),Rq,Rqx);Rq=Rqx.
analyseExp(tple(_,Els),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExps(Els,Df,Dfx,Rq,Rqx,Cand).
analyseExp(apply(_,Op,Arg,_),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Op,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Arg,Df1,Dfx,Rq1,Rqx,Cand).
analyseExp(dot(_,_,Op,_),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Op,Df,Dfx,Rq,Rqx,Cand).
analyseExp(where(_,Exp,Cond),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Exp,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Cond,Df1,Dfx,Rq1,Rqx,Cand).
analyseExp(conj(_,Lhs,Rhs),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Lhs,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Rhs,Df1,Dfx,Rq1,Rqx,Cand).
analyseExp(disj(_,Lhs,Rhs),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Lhs,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Rhs,Df1,Dfx,Rq1,Rqx,Cand).
analyseExp(neg(_,Rhs),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Rhs,Df,Dfx,Rq,Rqx,Cand).
analyseExp(cond(_,Tst,Lhs,Rhs,_),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Tst,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Lhs,Df1,Df2,Rq1,Rq2,Cand),
  analyseExp(Rhs,Df2,Dfx,Rq2,Rqx,Cand).
analyseExp(match(_,Lhs,Rhs),Df,Dfx,Rq,Rqx,Cand) :-
  analysePtn(Lhs,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Rhs,Df1,Dfx,Rq1,Rqx,Cand).
analyseExp(search(_,Lhs,Rhs,_),Df,Dfx,Rq,Rqx,Cand) :-
  analysePtn(Lhs,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Rhs,Df1,Dfx,Rq1,Rqx,Cand).
analyseExp(assertion(_,Rhs),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Rhs,Df,Dfx,Rq,Rqx,Cand).
analyseExp(show(_,Rhs),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Rhs,Df,Dfx,Rq,Rqx,Cand).
analyseExp(lambda(_,Rle,_),Df,Dfx,Rq,Rqx,Cond) :-
  analyseRl(Rle,Df,Dfx,Rq,Rqx,Cond).
analyseExp(theta(_Lc,_Path,_Defs,_Others,_Types,_Sig),Dfx,Dfx,Rqx,Rqx,_Cand).
analyseExp(record(_Lc,_Path,_Defs,_Others,_Types,_Sig),Dfx,Dfx,Rqx,Rqx,_Cand).
analyseExp(letExp(_,Th,Exp),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Th,Df,Df0,Rq,Rq0,Cand),
  analyseExp(Exp,Df0,Dfx,Rq0,Rqx,Cand).
analyseExp(enm(_,_,_),Dfx,Dfx,Rqx,Rqx,_).
analyseExp(cons(_,_,_),Dfx,Dfx,Rqx,Rqx,_).
analyseExp(intLit(_,_),Dfx,Dfx,Rqx,Rqx,_).
analyseExp(floatLit(_,_),Dfx,Dfx,Rqx,Rqx,_).
analyseExp(stringLit(_,_),Dfx,Dfx,Rqx,Rqx,_).

analysePtn(v(Lc,Nm,Tp),Df,Dfx,Rq,Rq,Cand) :-
  is_member(v(_,Nm,_),Cand) ->
    addVar(v(Lc,Nm,Tp),Df,Dfx) ; Df=Dfx.
analysePtn(tple(_,Els),Df,Dfx,Rq,Rqx,Cand) :-
  analysePtns(Els,Df,Dfx,Rq,Rqx,Cand).
analysePtn(apply(_,Op,Arg,_),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Op,Df,Df1,Rq,Rq1,Cand),
  analysePtn(Arg,Df1,Dfx,Rq1,Rqx,Cand).
analysePtn(where(_,P,C),Df,Dfx,Rq,Rqx,Cand) :-
  analysePtn(P,Df,Df1,Rq,Rq1,Cand),
  analyseExp(C,Df1,Dfx,Rq1,Rqx,Cand).
analysePtn(P,Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(P,Df,Dfx,Rq,Rqx,Cand).

analyseRl(equation(_,A,Cond,Value),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(A,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Cond,Df1,Df2,Rq1,Rq2,Cand),
  analyseExp(Value,Df2,Dfx,Rq2,Rqx,Cand).

analyseMany([],_,Df,Df,Rq,Rq,_).
analyseMany([P|Els],Call,Df,Dfx,Rq,Rqx,Cand) :-
  call(Call,P,Df,Df1,Rq,Rq1,Cand),
  analyseMany(Els,Call,Df1,Dfx,Rq1,Rqx,Cand).

analysePtns(Ptns,Df,Dfx,Rq,Rqx,Cand) :-
  analyseMany(Ptns,cnc:analysePtn,Df,Dfx,Rq,Rqx,Cand).

analyseExps(Exps,Df,Dfx,Rq,Rqx,Cand) :-
  analyseMany(Exps,cnc:analyseExp,Df,Dfx,Rq,Rqx,Cand).

analyseRules(Rls,Df,Dfx,Rq,Rqx,Cand) :-
  analyseMany(Rls,cnc:analyseRl,Df,Dfx,Rq,Rqx,Cand).

addVar(v(_,Nm,_),D,D) :- is_member(v(_,Nm,_),D),!.
addVar(V,D,[V|D]).




% transCond(given(Lc1,apply(Lc,Op,Args),)
