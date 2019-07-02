:- module(cnc,[genCondition/7]).

:- use_module(wff).
:- use_module(types).
:- use_module(canon).
:- use_module(unify).
:- use_module(abstract).
:- use_module(terms).
:- use_module(misc).
:- use_module(do).
:- use_module(errors).


/*
 * Ptn in Src
 * becomes
 * let{
 *  sF(Ptn,St) => AddEl(X,St).
 *  sF(_,St) default => do { return St}.
 * } in _iter(Src,Initial,sF)
 *
 * where AddEl, InitState are parameters to the conversion
 */
genCondition(search(Lc,Ptn,Src,Iterator),Path,Lift,_Seq,Succ,Initial,Exp) :-
  typeOfCanon(Ptn,PtnTp),
  genNme(Lc,PtnTp,"_",Anon),
  typeOfCanon(Iterator,ItrTp),
  typeOfCanon(Src,SrcTp),
  newTypeVar("_strm",RsltTp),
  genNme(Lc,RsltTp,"_st",St),
  call(Succ,unlifted(St),AddToFront),
  splitPtn(Ptn,Pttrn,PtnCond),
  call(Lift,unlifted(St),Dflt),
  typeOfCanon(AddToFront,MdlTp),
  FnTp = funType(tupleType([PtnTp,RsltTp]),MdlTp),
  % entangle type of iterator with the monad
  sameType(funType(tupleType([SrcTp,MdlTp,FnTp]),MdlTp),ItrTp,[]),
%  reportMsg("iterator type: %s",[ItrTp]),
%  reportMsg("local fun type: %s",[FnTp]),
  genstr("f",Fn),
  genNewName(Path,"Γ",ThPath),
  packageVarName(ThPath,Fn,LclName),

  FF=funDef(Lc,Fn,LclName,FnTp,[],[
    equation(Lc,tple(Lc,[Pttrn,St]),PtnCond,AddToFront),
    equation(Lc,tple(Lc,[Anon,St]),enm(Lc,"true",type("star.core*boolean")),Dflt)
  ]),
  Let = letExp(Lc,theta(Lc,ThPath,true,[FF],[],[],faceType([],[])),v(Lc,Fn,FnTp)),
  call(Lift,Initial,Init),
  Exp = apply(Lc,Iterator,tple(Lc,[Src,Init,Let]),MdlTp).

genCondition(conj(_Lc,A,B),Path,Lift,Seq,Succ,Initial,Exp) :-
  genCondition(A,Path,Lift,Seq,cnc:genCondition(B,Path,Lift,Seq,Succ),Initial,Exp).
genCondition(disj(_,A,B),Path,Lift,Seq,Succ,Initial,Exp) :-
  genCondition(A,Path,Lift,Seq,Succ,Initial,E1),
  genCondition(B,Path,Lift,Seq,Succ,lifted(E1),Exp).
genCondition(neg(Lc,N),Path,Lift,Seq,Succ,Initial,Exp) :-
  negatedCondition(Lc,N,Path,Lift,Seq,Succ,Initial,Exp).
genCondition(implies(Lc,G,T),Path,Lift,Seq,Succ,Initial,Exp) :-
  genCondition(neg(Lc,conj(Lc,G,neg(Lc,T))),Path,Lift,Seq,Succ,Initial,Exp).
% Other form of condition is treated similarly to a match
genCondition(Other,_Path,Lift,Seq,Succ,Initial,Exp) :-
  call(Succ,Initial,AddToSucc),
  newTypeVar("_",StTp),
  genNme(Lc,StTp,"_",St),
  typeOfCanon(AddToSucc,Tp),
  locOfCanon(Other,Lc),
  call(Lift,Initial,Init),
  call(Seq,St,Initial,cond(Lc,Other,AddToSucc,Init,Tp),Exp).

negatedCondition(Lc,Qury,Path,Lift,Seq,Succ,Initial,Exp) :-
  LogicalTp = type("star.core*boolean"),
  genCondition(Qury,Path,Lift,Seq,
	       cnc:mkLogical(Lc,Lift,"true"),
	       unlifted(enm(Lc,"false",LogicalTp)),
	       Negated),

  genNme(Lc,LogicalTp,"_",St),
  call(Succ,Initial,SuccCase),
  call(Lift,Initial,Init),
  typeOfCanon(Init,Tp),

  call(Seq,St,lifted(Negated),cond(Lc,St,Init,SuccCase,Tp),Exp).
%  reportMsg("negated condition -> %s",[Exp]).

mkLogical(Lc,Lift,Case,_,Deflt) :-
  call(Lift,unlifted(enm(Lc,Case,type("star.core*boolean"))),Deflt).

typeOfInitial(lifted(C),T) :-!,
  typeOfCanon(C,T).
typeOfInitial(unlifted(C),T) :-
  typeOfCanon(C,T).

genNme(Lc,Tp,Pr,v(Lc,Nm,Tp)) :-
  genstr(Pr,Nm).

passSt(_,St,St).

analyseExp(v(Lc,Nm,Tp),Dfx,Dfx,Rq,Rqx,Cand) :-
  is_member(v(_,Nm,_),Cand) -> addVar(v(Lc,Nm,Tp),Rq,Rqx);Rq=Rqx.
analyseExp(tple(_,Els),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExps(Els,Df,Dfx,Rq,Rqx,Cand).
analyseExp(apply(_,Op,Arg,_),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Op,Df,Df1,Rq,Rq1,Cand),
  analyseExp(Arg,Df1,Dfx,Rq1,Rqx,Cand).
analyseExp(dot(_,_,Op,_),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Op,Df,Dfx,Rq,Rqx,Cand).
analyseExp(varRef(_,Rhs),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Rhs,Df,Dfx,Rq,Rqx,Cand).
analyseExp(assign(_,Lhs,Rhs),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Lhs,Df,Df0,Rq,Rq1,Cand),
  analyseExp(Rhs,Df0,Dfx,Rq1,Rqx,Cand).
analyseExp(cell(_,Lhs),Df,Dfx,Rq,Rqx,Cand) :-
  analyseExp(Lhs,Df,Dfx,Rq,Rqx,Cand).
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
  analyseRule(Rle,Df,Dfx,Rq,Rqx,Cond).
analyseExp(theta(_Lc,_Path,_,_Defs,_Others,_Types,_Sig),Dfx,Dfx,Rqx,Rqx,_Cand).
analyseExp(record(_Lc,_Path,_,_Defs,_Others,_Types,_Sig),Dfx,Dfx,Rqx,Rqx,_Cand).
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

analyseRule(equation(_,A,Cond,Value),Df,Dfx,Rq,Rqx,Cand) :-
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

addVar(v(_,Nm,_),D,D) :- is_member(v(_,Nm,_),D),!.
addVar(V,D,[V|D]).
