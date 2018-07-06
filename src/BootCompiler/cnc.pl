:- module(cnc,[transRule/3,cncPredType/2]).

:- use_module(wff).
:- use_module(types).
:- use_module(canon).
:- use_module(unify).
:- use_module(abstract).
:- use_module(terms).


cncPredType(At,Tp) :- deRef(Tp,tpExp(tpFun("{}",1),At)).

checkRule(Lc,H,C,Tp,Defs,Defsx,Df,Dfx,E,Path) :-
  cncPredType(At,Tp),
  splitHead(H,_,A,IsDeflt),
  pushScope(E,Env),
  typeOfArgPtn(A,AT,Env,E0,Args,Path),
  findType("boolean",Lc,Env,LogicalTp),
  typeOfExp(C,LogicalTp,E0,E1,Cond,Path),
  dischargeConstraints(E,E1),
  (IsDeflt=isDeflt -> Defs=Defsx, Df=[rule(Lc,Args,Cond)|Dfx]; Defs=[rule(Lc,Args,Cond)|Defsx],Df=Dfx).
checkRule(Lc,_,_,_,ProgramType,Defs,Defs,Df,Df,_,_) :-
  reportError("rule not consistent with expected type: %s",[ProgramType],Lc).
