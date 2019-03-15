:- module(cnc,[genSearch/3,genIterableGl/3,genCondition/8]).

:- use_module(wff).
:- use_module(types).
:- use_module(canon).
:- use_module(unify).
:- use_module(abstract).
:- use_module(terms).
:- use_module(freevars).
:- use_module(misc).
:- use_module(do).
:- use_module(errors).

/*
 * Ptn in Src -- as a standalone condition
 *
 * becomes:
 *
 * some(Ptnv) .= _iter(Src,none, let{
 *  sF(Ptn,none) =>  some(Ptnv)
 *  sF(_,St) => St
 * } in sF))
 */

genSearch(search(Lc,Ptn,Src,Iterator),Path,Gl) :-
  pickupContract(Lc,Env,"execution",ExTp,ErTp,ExOp),
  findType("action",Lc,Env,ActionTp),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfCanon(Ptn,PtnTp),
  splitPtn(Ptn,Pttrn,PtnCond),
  
  genNme(Lc,StTp,"_st",St),
  genNme(Lc,PtnTp,"_",Anon),
  genNewName(Path,"Γ",ThPath),
  genstr("sF",Fn),
  packageVarName(ThPath,Fn,LclName),
  Initial = enm(Lc,"noneFound",StTp),
  FnTp = funType(tupleType([PtnTp,StTp]),StTp),
  CnsTp =  funType(tupleType([PtnTp]),StTp),
  FF=funDef(Lc,Fn,LclName,FnTp,[],[
    equation(Lc,tple(Lc,[Pttrn,St]),PtnCond,
        apply(Lc,v(Lc,"noMore",CnsTp),tple(Lc,[Pttrn]),StTp)),
    equation(Lc,tple(Lc,[Anon,St]),enm(Lc,"true",type("star.core*boolean")),St)
  ]),
  Let = letExp(Lc,theta(Lc,ThPath,true,[FF],[],[],faceType([],[])),v(Lc,Fn,FnTp)),
  Exp = apply(Lc,Iterator,tple(Lc,[Src,Let,Initial]),StTp),
  Gl = match(Lc,apply(Lc,v(Lc,"noMore",CnsTp),tple(Lc,[Pttrn]),StTp),Exp).


  /*
   * 'iterable' conditions become a match on the result of a search
   *
   * becomes:
   *
   * some(PtnV) .= <genCondition>(C,none,...)
   */

genIterableGl(Cond,Path,Gl) :-
  locOfCanon(Cond,Lc),
  pickupContract(Lc,Env,"execution",ExTp,ErTp,ExOp),
  findType("boolean",Lc,Env,LogicalTp),
  findType("option",Lc,Env,OptionTp),


  
  goalVars(Cond,Vrs),
  VTpl = tple(Lc,Vrs),
  typeOfCanon(VTpl,ETp),
  IterTp = tpExp(tpFun("star.iterable*iterState",1),ETp),
  InitState = enm(Lc,"noneFound",IterTp),
  genCondition(Cond,Path,cnc:setEl(Lc,VTpl,ETp,IterTp),cnc:passSt(Lc),InitState,Seq),
  Gl = match(Lc,apply(Lc,v(Lc,"noMore",funType(tupleType([ETp]),IterTp)),tple(Lc,[VTpl]),IterTp),Seq).

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
genCondition(search(Lc,Ptn,Src,Iterator),Path,Lift,_Seq,Succ,Fail,Initial,Exp) :-
  typeOfCanon(Ptn,PtnTp),
  genNme(Lc,PtnTp,"_",Anon),
  typeOfCanon(Iterator,ItrTp),
  typeOfCanon(Src,SrcTp),
  newTypeVar("_strm",RsltTp),
  genNme(Lc,RsltTp,"_st",St),
  call(Succ,unlifted(St),AddToFront),
  splitPtn(Ptn,Pttrn,PtnCond),
  call(Fail,unlifted(St),Dflt),
  typeOfCanon(AddToFront,MdlTp),
  FnTp = funType(tupleType([PtnTp,RsltTp]),MdlTp),
  % entangle type of iterator with the monad
  sameType(funType(tupleType([SrcTp,MdlTp,FnTp]),MdlTp),ItrTp,[]),
%  reportMsg("iterator type: %s",[ItrTp]),
`  reportMsg("local fun type: %s",[FnTp]),
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

/*
  * Key->Ptn in Src
  * becomes
  * let{
  *  sF(Key,Ptn,St) => AddEl(X,St).
  *  sF(_,_,St) default => St.
  * } in ixiterate(Src,sF,Initial)
  *
  * where AddEl, InitState are parameters to the conversion
*/
genCondition(ixsearch(Lc,Key,Ptn,Src,Iterator),Path,Lift,_Seq,Succ,Fail,Initial,Exp) :-
  typeOfCanon(Ptn,PtnTp),
  typeOfCanon(Key,KyTp),

  genNme(Lc,PtnTp,"_",Anon),

  typeOfCanon(Iterator,ItrTp),
  typeOfCanon(Src,SrcTp),
  genNme(Lc,RsltTp,"_st",St),

  splitPtn(Ptn,Pttrn,PtnCond),
  splitPtn(Key,KPtrn,KeyCond),
  mergeGl(KeyCond,PtnCond,Lc,IxCond),

  call(Succ,unlifted(St),AddToFront),
  call(Fail,unlifted(St),Dflt),
  typeOfCanon(AddToFront,MdlTp),
  newTypeVar("_strm",RsltTp),
  FnTp = funType(tupleType([KyTp,PtnTp,RsltTp]),MdlTp),
  % entangle type of iterator with the monad
  sameType(funType(tupleType([SrcTp,MdlTp,FnTp]),MdlTp),ItrTp,[]),

  genNme(Lc,KyTp,"_k",KAnon),
  genstr("f",Fn),
  genNewName(Path,"Γ",ThPath),
  packageVarName(ThPath,Fn,LclName),

  FF=funDef(Lc,Fn,LclName,FnTp,[],
	    [
	     equation(Lc,tple(Lc,[KPtrn,Pttrn,St]),IxCond,AddToFront),
	     equation(Lc,tple(Lc,[KAnon,Anon,St]),enm(Lc,"true",type("star.core*boolean")),Dflt)
	    ]),
  Let = letExp(Lc,theta(Lc,ThPath,true,[FF],[],[],faceType([],[])),v(Lc,Fn,FnTp)),
  call(Lift,Initial,Init),
  Exp = apply(Lc,Iterator,tple(Lc,[Src,Init,Let]),MdlTp).


/*
 * Ptn .= Expr
 * becomes
 * (Init)>>=(St)=>(Ptn.=Expr?Succ(St)||Fail(St)
 */
genCondition(match(Lc,Ptn,Exp),_Path,_Lift,Seq,Succ,Fail,Initial,Exp) :-
  call(Succ,unlifted(St),AddToSucc),
  call(Fail,unlifted(St),AddToFail),
  typeOfCanon(AddToSucc,Tp),
  call(Seq,St,Initial,cond(Lc,match(Lc,Ptn,Exp),AddToSucc,AddToFail,Tp),Exp).

genCondition(conj(_Lc,A,B),Path,Lift,Seq,Succ,Fail,Initial,Exp) :-
  genCondition(A,Path,Lift,Seq,cnc:genCondition(B,Path,Lift,Seq,Succ,Fail),Fail,Initial,Exp).
genCondition(disj(_,A,B),Path,Lift,Seq,Succ,Fail,Initial,Exp) :-
  genCondition(A,Path,Lift,Seq,Succ,Fail,Initial,E1),
  genCondition(B,Path,Lift,Seq,Succ,Fail,lifted(E1),Exp).
genCondition(neg(_,A),Path,Lift,Seq,Succ,Fail,Initial,Exp) :-
  genCondition(A,Path,Lift,Seq,Fail,Succ,Initial,Exp).
genCondition(implies(Lc,G,T),Path,Lift,Seq,Succ,Fail,Initial,Exp) :-
  genCondition(neg(Lc,conj(Lc,G,neg(Lc,T))),Path,Lift,Seq,Succ,Fail,Initial,Exp).
% Other form of condition is treated similarly to a match
genCondition(Other,_Path,_Lift,Seq,Succ,Fail,Initial,Exp) :-
  locOfCanon(Other,Lc),
  newTypeVar("_",StTp),
  genNme(Lc,StTp,"_st",St),
  call(Succ,unlifted(St),AddToSucc),
  call(Fail,unlifted(St),AddToFail),
  typeOfCanon(AddToSucc,Tp),
  call(Seq,St,Initial,cond(Lc,Other,AddToSucc,AddToFail,Tp),Exp).

typeOfInitial(lifted(C),T) :-!,
  typeOfCanon(C,T).
typeOfInitial(unlifted(C),T) :-
  typeOfCanon(C,T).

genNme(Lc,Tp,Pr,v(Lc,Nm,Tp)) :-
  genstr(Pr,Nm).

genEl(Lc,El,Gen,StTp,St,apply(Lc,Gen,tple(Lc,[El,St]),StTp)).

setEl(Lc,El,ElTp,StTp,_St,apply(Lc,v(Lc,"noMore",funType(tupleType([ElTp]),StTp)),tple(Lc,[El]),StTp)).

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
