:- module(resolve,[overload/4]).

:- use_module(dict).
:- use_module(declmgt).
:- use_module(meta).
:- use_module(misc).
:- use_module(errors).
:- use_module(types).
:- use_module(freshen).
:- use_module(canon).
:- use_module(unify).
:- use_module(location).

overload(Defs,Dict,Opts,RDefs) :-
  map(Defs,resolve:overloadDef(Dict,Opts),RDefs).

overloadDef(Dict,Opts,funDef(Lc,Nm,ExtNm,H,Tp,Cx,Eqns),RF) :-!,
  overloadFunction(Lc,Nm,ExtNm,H,Tp,Cx,Eqns,Dict,Opts,RF).
overloadDef(Dict,Opts,varDef(Lc,Nm,ExtNm,Cx,Tp,Value),RD) :-!,
  overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Value,Dict,Opts,RD).
overloadDef(_,_,Def,Def).

overloadFunction(Lc,Nm,ExtNm,H,Tp,[],Eqns,Dict,Opts,funDef(Lc,Nm,ExtNm,H,Tp,[],REqns)) :-
  overloadEquations(Eqns,Dict,Opts,[],REqns),!.
overloadFunction(Lc,Nm,ExtNm,H,Tp,Cx,Eqns,Dict,Opts,funDef(Lc,Nm,ExtNm,H,Tp,[],REqns)) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadEquations(Eqns,FDict,Opts,CVars,REqns),!.

overloadEquations(Eqns,Dict,Opts,Extra,REqns) :-
  overloadList(Eqns,overloadEquation(Extra),Dict,Opts,REqns).

overloadEquation(Extra,rule(Lc,Args,G,Exp),Dict,Opts,rule(Lc,RArgs,RG,RExp)) :-
  defineArgVars(Args,Dict,RDict),
  resolveTerm(Args,RDict,Opts,RA),
  addExtra(Extra,RA,RArgs),
  resolveGuard(G,RDict,Opts,RG),
  resolveTerm(Exp,RDict,Opts,RExp),
  checkOpt(Opts,traceCheck,meta:showMsg(Lc,"overloaded equation %s",[rle(rule(Lc,RArgs,RG,RExp))])).

resolveGuard(none,_,_,none) :-!.
resolveGuard(some(G),Dict,Opts,some(RG)) :-
  resolveTerm(G,Dict,Opts,RG).

% These are used when resolving lambdas only. A lambda cannot introduce any dictionary variables
overloadRule(Over,rule(Lc,Args,G,Exp),Dict,Opts,St,Stx,rule(Lc,RArgs,RG,RExp)) :-
  defineArgVars(Args,Dict,RDict),
  overloadTerm(Args,RDict,Opts,St,St0,RArgs),
  overloadGuard(G,RDict,Opts,St0,St1,RG),
  call(Over,Exp,RDict,Opts,St1,Stx,RExp).

overloadDefn(Lc,Nm,ExtNm,[],Tp,Exp,Dict,Opts,varDef(Lc,Nm,ExtNm,[],Tp,RExp)) :-
  resolveTerm(Exp,Dict,Opts,RExp).
overloadDefn(Lc,Nm,ExtNm,Cx,Tp,Exp,Dict,
	     Opts,varDef(Lc,Nm,ExtNm,[],Tp,
			 lambda(Lc,Lbl,[],rule(Lc,tple(Lc,CVars),none,RExp),OTp))) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  contractTypes(Cx,Tps),
  makeContractFunType(Tp,Tps,OTp),
  resolveTerm(Exp,FDict,Opts,RExp),
  lambdaLbl(Nm,"over",Lbl).

makeContractFunType(allType(V,T),Cx,allType(V,CT)) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(constrained(T,_C),Cx,CT) :-
  makeContractFunType(T,Cx,CT).
makeContractFunType(T,Cx,funType(tplType(Cx),T)).

defineCVars(_,[],Dict,[],Dict).
defineCVars(Lc,[implementsFace(X,faceType(Flds,_))|Cx],Dict,CVars,FDict) :-
  fieldVars(Lc,X,Flds,CVars,CVrs,Dict,CDict),
  defineCVars(Lc,Cx,CDict,CVrs,FDict).
defineCVars(Lc,[implicit(Nm,Tp)|Cx],Dict,[v(Lc,Nm,Tp)|CVars],FDict) :-
  declareVr(Lc,Nm,Tp,none,Dict,DDict),
  defineCVars(Lc,Cx,DDict,CVars,FDict).
defineCVars(Lc,[Con|Cx],Dict,[v(Lc,CVarNm,ConTp)|CVars],FDict) :-
  implementationName(Con,ImplNm),
  mangleName("_",value,ImplNm,CVarNm),
  contractType(Con,ConTp),
  declareImplementation(ImplNm,CVarNm,ConTp,Dict,D0),
  declareVr(Lc,CVarNm,ConTp,none,D0,D1),
  defineCVars(Lc,Cx,D1,CVars,FDict).

fieldVars(_,_,[],CVars,CVars,Dict,Dict).
fieldVars(Lc,Tp,[(FldNm,FldTp)|Flds],[NV|CVrs],XVrs,Dict,CDict) :-
  genVar(FldNm,Lc,funType(tplType([Tp]),FldTp),NV),
  fieldVars(Lc,Tp,Flds,CVrs,XVrs,[access(FldNm,NV)|Dict],CDict).

defineArgVars(tple(_,Args),Dict,RDict) :-
  rfold(Args,resolve:defineArg,Dict,RDict).
defineArgVars(_,Dict,Dict).

defineArg(v(Lc,Nm,Tp),Dict,RDict) :-!,
  declareVr(Lc,Nm,Tp,none,Dict,RDict).
defineArg(capply(_,_,Args,_),Dict,RDict) :-!,
  defineArgVars(Args,Dict,RDict).
defineArg(_,Dict,Dict).
  
resolveTerm(Term,Dict,Opts,Resolved) :-
  locOfCanon(Term,Lc),
  traceCheck(Opts,Lc,"resolve %s",[can(Term)]),
  overloadTerm(Term,Dict,Opts,inactive,St,RTerm),!,
  traceCheck(Opts,Lc,"resolved term %s",[can(RTerm)]),
  resolveAgain(inactive,St,Term,RTerm,Dict,Opts,Resolved).

% Somewhat complex logic to allow multiple iterations unless it will not help
resolveAgain(Prior,resolved,Term,T,Dict,Opts,R) :- !,
  overloadTerm(T,Dict,Opts,Prior,St,T0),
  resolveAgain(inactive,St,Term,T0,Dict,Opts,R).
resolveAgain(_,inactive,_,T,_,_,T) :- !.
resolveAgain(active(_,Msg),active(Lc,Msg1),Term,_,_,_,Term) :-
  similarStrings(Msg,Msg1),
  reportError("cannot resolve %s because %s",[can(Term),ss(Msg)],Lc).
resolveAgain(_,fatal(Lc,Msg),Term,_,_,_,Term) :-
  reportError("cannot resolve %s because %s",[can(Term),ss(Msg)],Lc).
resolveAgain(_,active(Lc,Msg),Orig,_,Dict,Opts,R) :-
  overloadTerm(Orig,Dict,Opts,inactive,St,T0),!,
  resolveAgain(active(Lc,Msg),St,Orig,T0,Dict,Opts,R).

markActive(_,Lc,Msg,active(Lc,Msg)).

markFatal(_,Lc,Msg,fatal(Lc,Msg)).

markResolved(inactive,resolved).
markResolved(St,St).

overloadTerm(void,_,_,St,St,void).
overloadTerm(v(Lc,Nm,Tp),_,_,St,St,v(Lc,Nm,Tp)).
overloadTerm(anon(Lc,Tp),_,_,St,St,anon(Lc,Tp)).
overloadTerm(intLit(Lc,Ix),_,_,St,St,intLit(Lc,Ix)).
overloadTerm(bigLit(Lc,Ix),_,_,St,St,bigLit(Lc,Ix)).
overloadTerm(floatLit(Lc,Dx),_,_,St,St,floatLit(Lc,Dx)).
overloadTerm(charLit(Lx,Sx),_,_,St,St,charLit(Lx,Sx)).
overloadTerm(stringLit(Lx,Sx),_,_,St,St,stringLit(Lx,Sx)).
overloadTerm(dot(Lc,Rc,Fld,Tp),Dict,Opts,St,Stx,Dot) :-
  overloadTerm(Rc,Dict,Opts,St,St0,RRc),
  resolveDot(Lc,RRc,Fld,Tp,Dict,Opts,St0,Stx,Dot).
overloadTerm(update(Lc,Rc,Fld,Vl),Dict,Opts,St,Stx,Update) :-
  overloadTerm(Rc,Dict,Opts,St,St0,Rx),
  overloadTerm(Vl,Dict,Opts,St0,St1,Vx),
  resolveUpdate(Lc,Rx,Fld,Vx,Dict,Opts,St1,Stx,Update).
overloadTerm(tdot(Lc,Rc,Fld,Tp),Dict,Opts,St,Stx,Dot) :-
  overloadTerm(Rc,Dict,Opts,St,St0,RRc),
  resolveTDot(Lc,RRc,Fld,Tp,Dict,Opts,St0,Stx,Dot).
overloadTerm(enm(Lc,Rf,Tp),_,_,St,St,enm(Lc,Rf,Tp)).
overloadTerm(tple(Lc,Args),Dict,Opts,St,Stx,tple(Lc,RArgs)) :-!,
  overloadLst(Args,resolve:overloadTerm,Dict,Opts,St,Stx,RArgs).
overloadTerm(open(Lc,Er,Tp),Dict,Opts,St,Stx,open(Lc,Err,Tp)) :-
  overloadTerm(Er,Dict,Opts,St,Stx,Err).
overloadTerm(cell(Lc,Inn),Dict,Opts,St,Stx,cell(Lc,Inn1)) :-
  overloadTerm(Inn,Dict,Opts,St,Stx,Inn1).
overloadTerm(deref(Lc,Inn),Dict,Opts,St,Stx,deref(Lc,Inn1)) :-
  overloadTerm(Inn,Dict,Opts,St,Stx,Inn1).
overloadTerm(letExp(Lc,Decls,Defs,Bound),Dict,Opts,St,Stx,
	     letExp(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadTerm,Dict,Opts,St,Stx,RDefs,RBound).
overloadTerm(letRec(Lc,Decls,Defs,Bound),Dict,Opts,St,Stx,
	     letRec(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadTerm,Dict,Opts,St,Stx,RDefs,RBound).
overloadTerm(where(Lc,Trm,Cond),Dict,Opts,St,Stx,where(Lc,RTrm,RCond)) :-
  overloadTerm(Trm,Dict,Opts,St,St0,RTrm),
  overloadTerm(Cond,Dict,Opts,St0,Stx,RCond).
overloadTerm(conj(Lc,L,R),Dict,Opts,St,Stx,conj(Lc,RL,RR)) :-
  overloadTerm(L,Dict,Opts,Opts,St,St0,RL),
  overloadTerm(R,Dict,Opts,St0,Stx,RR).
overloadTerm(disj(Lc,L,R),Dict,Opts,St,Stx,disj(Lc,RL,RR)) :-
  overloadTerm(L,Dict,Opts,St,St0,RL),
  overloadTerm(R,Dict,Opts,St0,Stx,RR).
overloadTerm(cond(Lc,T,L,R,Tp),Dict,Opts,St,Stx,cond(Lc,RT,RL,RR,Tp)) :-
  overloadTerm(T,Dict,Opts,St,St0,RT),
  overloadTerm(L,Dict,Opts,St0,St1,RL),
  overloadTerm(R,Dict,Opts,St1,Stx,RR).
overloadTerm(implies(Lc,G,T),Dict,Opts,St,Stx,implies(Lc,RG,RT)) :-
  overloadTerm(G,Dict,Opts,St,St0,RG),
  overloadTerm(T,Dict,Opts,St0,Stx,RT).
overloadTerm(neg(Lc,T),Dict,Opts,St,Stx,neg(Lc,RT)) :-
  overloadTerm(T,Dict,Opts,St,Stx,RT).
overloadTerm(match(Lc,L,R),Dict,Opts,St,Stx,match(Lc,RL,RR)) :-
  overloadTerm(L,Dict,Opts,St,St0,RL),
  overloadTerm(R,Dict,Opts,St0,Stx,RR).
overloadTerm(case(Lc,B,C,Tp),Dict,Opts,St,Stx,case(Lc,RB,RC,Tp)) :-
  overloadTerm(B,Dict,Opts,St,St0,RB),
  overloadCases(C,resolve:overloadTerm,Dict,Opts,St0,Stx,RC).
overloadTerm(apply(ALc,over(Lc,T,Cx),Args,Tp),Dict,Opts,St,Stx,Term) :-
  overloadMethod(ALc,Lc,T,Cx,Args,Tp,makeApply,Dict,Opts,St,Stx,Term).
overloadTerm(apply(ALc,overaccess(Lc,T,RcTp,Fld,FTp),Args,ATp),Dict,Opts,St,Stx,Term) :-
  overloadAccess(ALc,Lc,T,RcTp,Fld,FTp,Args,ATp,Dict,Opts,St,Stx,Term).
overloadTerm(apply(Lc,Op,Args,Tp),Dict,Opts,St,Stx,apply(Lc,ROp,RArgs,Tp)) :-
  overloadTerm(Op,Dict,Opts,St,St0,ROp),
  overloadTerm(Args,Dict,Opts,St0,Stx,RArgs).
overloadTerm(capply(Lc,Op,Args,Tp),Dict,Opts,St,Stx,capply(Lc,ROp,RArgs,Tp)) :-
  overloadTerm(Op,Dict,Opts,St,St0,ROp),
  overloadTerm(Args,Dict,Opts,St0,Stx,RArgs).
overloadTerm(tapply(ALc,over(Lc,T,Cx),Args,Tp,ErTp),Dict,Opts,St,Stx,Term) :-
  overloadMethod(ALc,Lc,T,Cx,Args,Tp,makeTApply(ErTp),Dict,Opts,St,Stx,Term).
overloadTerm(tapply(Lc,Op,Args,Tp,ErTp),Dict,Opts,St,Stx,tapply(Lc,ROp,RArgs,Tp,ErTp)) :-
  overloadTerm(Op,Dict,Opts,St,St0,ROp),
  overloadTerm(Args,Dict,Opts,St0,Stx,RArgs).
overloadTerm(over(Lc,T,Cx),Dict,Opts,St,Stx,Over) :-
  overloadOver(Lc,T,Cx,Dict,Opts,St,Stx,makeApply,Over).
overloadTerm(overaccess(Lc,T,RcTp,Fld,FTp),Dict,Opts,St,Stx,Over) :-
  resolveAccess(Lc,RcTp,Fld,FTp,Dict,Opts,St,St1,AccessOp),
  resolveRef(T,[AccessOp],[],OverOp,Dict,Opts,St1,St2,NArgs),
  curryOver(Lc,OverOp,NArgs,funType(tplType([RcTp]),FTp),makeApply,Over),
  markResolved(St2,Stx);
  genMsg("cannot find accessor for %s of type %s",[ss(Fld),tpe(RcTp)],Msg),
  markFatal(St,Lc,Msg,Stx),
  Over = overaccess(Lc,T,RcTp,Fld,FTp).
overloadTerm(mtd(Lc,Nm,Tp),_,_,St,Stx,mtd(Lc,Nm,Tp)) :-
  genMsg("cannot find implementation for %s",[Nm],Msg),
  markActive(St,Lc,Msg,Stx).
overloadTerm(lambda(Lc,Lbl,Cx,Eqn,Tp),Dict,Opts,St,Stx,Lam) :-!,
  overloadLambda(Lc,Lbl,Cx,Eqn,Tp,Dict,Opts,St,Stx,Lam).
overloadTerm(valof(Lc,A,Tp),Dict,Opts,St,Stx,valof(Lc,AA,Tp)) :-!,
  overloadAction(A,Dict,Opts,St,Stx,AA).
overloadTerm(task(Lc,A,Tp),Dict,Opts,St,Stx,task(Lc,AA,Tp)) :-!,
  overloadTerm(A,Dict,Opts,St,Stx,AA).
overloadTerm(suspend(Lc,T,M,Tp),Dict,Opts,St,Stx,suspend(Lc,TT,MM,Tp)) :-!,
  overloadTerm(T,Dict,Opts,St,St0,TT),
  overloadTerm(M,Dict,Opts,St0,Stx,MM).
overloadTerm(retire(Lc,T,M,Tp),Dict,Opts,St,Stx,retire(Lc,TT,MM,Tp)) :-!,
  overloadTerm(T,Dict,Opts,St,St0,TT),
  overloadTerm(M,Dict,Opts,St0,Stx,MM).
overloadTerm(resume(Lc,T,M,Tp),Dict,Opts,St,Stx,resume(Lc,TT,MM,Tp)) :-!,
  overloadTerm(T,Dict,Opts,St,St0,TT),
  overloadTerm(M,Dict,Opts,St0,Stx,MM).
overloadTerm(thnkRef(Lc,A,Tp),Dict,Opts,St,Stx,thnkRef(Lc,AA,Tp)) :-!,
  overloadTerm(A,Dict,Opts,St,Stx,AA).
overloadTerm(newSV(Lc,Tp),_Dict,_Opts,Stx,Stx,newSV(Lc,Tp)) :-!.
overloadTerm(svGet(Lc,A,T),Dict,Opts,St,Stx,svGet(Lc,AA,T)) :-!,
  overloadTerm(A,Dict,Opts,St,Stx,AA).
overloadTerm(svSet(Lc,Th,Vl),Dict,Opts,St,Stx,svSet(Lc,TT,VV)) :-!,
  overloadTerm(Th,Dict,Opts,St,St0,TT),
  overloadTerm(Vl,Dict,Opts,St0,Stx,VV).
overloadTerm(try(Lc,E,ErTp,H),Dict,Opts,St,Stx,try(Lc,EE,ErTp,HH)) :-
  setTryScope(Dict,ErTp,D1),
  overloadTerm(E,D1,Opts,St,St1,EE),
  overloadCases(H,resolve:overloadTerm,Dict,Opts,St1,Stx,HH).
overloadTerm(throw(Lc,E,ErTp),Dict,Opts,St,Stx,throw(Lc,EE,ErTp)) :-
  overloadTerm(E,Dict,Opts,St,Stx,EE).
overloadTerm(T,_,_,St,St,T) :-
  locOfCanon(T,Lc),
  reportError("invalid term to resolve %s",[can(T)],Lc).

overloadGuard(none,_,_,Stx,Stx,none) :-!.
overloadGuard(some(G),Dict,Opts,St,Stx,some(RG)) :-
  overloadTerm(G,Dict,Opts,St,Stx,RG).

overloadLambda(Lc,Lbl,[],rule(Lc,Args,G,Exp),Tp,Dict,Opts,St,Stx,
	       lambda(Lc,Lbl,[],rule(Lc,RArgs,RG,RExp),Tp)) :-!,
  defineArgVars(Args,Dict,RDict),
  overloadTerm(Args,RDict,Opts,St,St0,RArgs),
  overloadGuard(G,RDict,Opts,St0,St1,RG),
  overloadTerm(Exp,RDict,Opts,St1,Stx,RExp).
overloadLambda(Lc,Lbl,Cx,Eqn,Tp,Dict,Opts,Stx,Stx,lambda(Lc,Lbl,[],OEqn,Tp)) :-
  defineCVars(Lc,Cx,Dict,CVars,FDict),
  overloadEquation(CVars,Eqn,FDict,Opts,OEqn).
					      
overloadLet(Lc,Decls,Defs,Bound,RR,Dict,Opts,St,Stx,RDefs,RBound) :-
  declareAllDecls(Decls,Lc,Dict,RDict),
  call(RR,Bound,RDict,Opts,St,Stx,RBound),
  overload(Defs,RDict,Opts,RDefs).

overloadAction(doNop(Lc),_,_,St,St,doNop(Lc)) :-!.
overloadAction(doSeq(Lc,A,B),Dict,Opts,St,Stx,doSeq(Lc,AA,BB)) :-
  overloadAction(A,Dict,Opts,St,St1,AA),
  overloadAction(B,Dict,Opts,St1,Stx,BB).
overloadAction(doLbld(Lc,Lb,A),Dict,Opts,St,Stx,doLbld(Lc,Lb,AA)) :-!,
  overloadAction(A,Dict,Opts,St,Stx,AA).
overloadAction(doBrk(Lc,Lb),_,_,St,St,doBrk(Lc,Lb)) :-!.
overloadAction(doValis(Lc,A),Dict,Opts,St,Stx,doValis(Lc,AA)) :-
  overloadTerm(A,Dict,Opts,St,Stx,AA).
overloadAction(doDefn(Lc,V,E),Dict,Opts,St,Stx,doDefn(Lc,V,EE)) :-
  overloadTerm(E,Dict,Opts,St,Stx,EE).
overloadAction(doMatch(Lc,P,A),Dict,Opts,St,Stx,doMatch(Lc,PP,AA)) :-
  overloadTerm(P,Dict,Opts,St,St1,PP),
  overloadTerm(A,Dict,Opts,St1,Stx,AA).
overloadAction(doAssign(Lc,P,A),Dict,Opts,St,Stx,doAssign(Lc,PP,AA)) :-
  overloadTerm(P,Dict,Opts,St,St1,PP),
  overloadTerm(A,Dict,Opts,St1,Stx,AA).
overloadAction(doTry(Lc,A,ErTp,H),Dict,Opts,St,Stx,doTry(Lc,AA,ErTp,HH)) :-
  setTryScope(Dict,ErTp,D1),
  overloadAction(A,D1,Opts,St,St1,AA),
  overloadCases(H,resolve:overloadAction,Dict,Opts,St1,Stx,HH).
overloadAction(doThrow(Lc,E),Dict,Opts,St,Stx,doThrow(Lc,EE)) :-
  overloadTerm(E,Dict,Opts,St,Stx,EE).
overloadAction(doIfThenElse(Lc,T,A,B),Dict,Opts,St,Stx,doIfThenElse(Lc,TT,AA,BB)) :-
  overloadTerm(T,Dict,Opts,St,St1,TT),
  overloadAction(A,Dict,Opts,St1,St2,AA),
  overloadAction(B,Dict,Opts,St2,Stx,BB).
overloadAction(doCase(Lc,B,C,Tp),Dict,Opts,St,Stx,doCase(Lc,RB,RC,Tp)) :-
  overloadTerm(B,Dict,Opts,St,St0,RB),
  overloadCases(C,resolve:overloadAction,Dict,Opts,St0,Stx,RC).
overloadAction(doWhile(Lc,T,A),Dict,Opts,St,Stx,doWhile(Lc,TT,AA)) :-
  overloadTerm(T,Dict,Opts,St,St1,TT),
  overloadAction(A,Dict,Opts,St1,Stx,AA).
overloadAction(doLet(Lc,Decls,Defs,Bound),Dict,Opts,St,Stx,doLet(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadAction,Dict,Opts,St,Stx,RDefs,RBound).
overloadAction(doLetRec(Lc,Decls,Defs,Bound),Dict,Opts,St,Stx,doLetRec(Lc,Decls,RDefs,RBound)) :-
  overloadLet(Lc,Decls,Defs,Bound,resolve:overloadAction,Dict,Opts,St,Stx,RDefs,RBound).
overloadAction(case(Lc,G,C,Tp),Dict,Opts,St,Stx,case(Lc,GG,CC,Tp)) :-
  overloadTerm(G,Dict,Opts,St,St1,GG),
  overloadCases(C,resolve:overloadAction,Dict,Opts,St1,Stx,CC).
overloadAction(doExp(Lc,T),Dict,Opts,St,Stx,doExp(Lc,TT)) :-
  overloadTerm(T,Dict,Opts,St,Stx,TT).
overloadAction(A,_,_,St,St,A) :-
  locOfCanon(A,Lc),
  reportError("cannot resolve action %s",[cnact(A)],Lc).

overloadOver(Lc,T,Cx,Dict,Opts,St,Stx,Make,Over) :-
  resolveConstraint(Lc,Cx,Dict,Opts,St,St0,DTerms,[]),!,
  resolveRef(T,DTerms,[],OverOp,Dict,Opts,St0,St1,NArgs),
  typeOfCanon(T,TTp),
  overApply(Lc,OverOp,NArgs,TTp,Make,Over),
  markResolved(St1,Stx).
overloadOver(Lc,T,Cx,_Dict,_Opts,St,Stx,_,over(Lc,T,Cx)) :-
  genMsg("cannot find implementation for contracts %s",[Cx],Msg),
  markActive(St,Lc,Msg,Stx).

overloadMethod(_ALc,Lc,T,Cx,Args,Tp,Make,Dict,Opts,St,Stx,Reslvd) :-
  checkOpt(Opts,traceCheck,meta:showMsg(Lc,"overload method: %s:%s",[can(apply(Lc,over(Lc,T,Cx),Args,Tp)),tpe(Tp)])),
  resolveConstraint(Lc,Cx,Dict,Opts,St,St0,DTerms,[]),
  markResolved(St0,St1),
  overloadTerm(Args,Dict,Opts,St1,St2,tple(_,RArgs)),
  resolveRef(T,DTerms,RArgs,OverOp,Dict,Opts,St2,Stx,NArgs),
  overApply(Lc,OverOp,NArgs,Tp,Make,Reslvd),
  checkOpt(Opts,traceCheck,meta:showMsg(Lc,"overload method resolved term %s",[can(Reslvd)])).

overloadAccess(ALc,Lc,T,RcTp,Fld,Tp,Args,ATp,Dict,Opts,St,Stx,
	       apply(ALc,Op,tple(LcA,NArgs),ATp)) :-
  resolveAccess(Lc,RcTp,Fld,Tp,Dict,Opts,St,St1,AccessOp),
  overloadTerm(Args,Dict,Opts,St1,St2,tple(LcA,RArgs)),
  resolveRef(T,[AccessOp],RArgs,Op,Dict,Opts,St2,Stx,NArgs).
  
overloadCases(Cses,Resolver,Dict,Opts,St,Stx,RCases) :-
  overloadLst(Cses,resolve:overloadRule(Resolver),Dict,Opts,St,Stx,RCases).

overApply(_,OverOp,[],_,_,OverOp) :- !.
overApply(Lc,OverOp,Args,Tp,Make,Over) :- \+isProgramType(Tp),!,
  call(Make,Lc,OverOp,Args,Tp,Over).
overApply(Lc,OverOp,Args,Tp,Make,Lam) :-
  curryOver(Lc,OverOp,Args,Tp,Make,Lam).

makeApply(Lc,Op,Args,Tp,apply(Lc,Op,tple(Lc,Args),Tp)).

makeTApply(ErTp,Lc,Op,Args,Tp,tapply(Lc,Op,tple(Lc,Args),Tp,ErTp)).

curryOver(Lc,OverOp,Cx,Tp,Make,
    lambda(Lc,Lbl,[],rule(Lc,tple(Lc,Args),none,
			  Call),funType(tplType(ArTps),Tp))) :-
  progArgTypes(Tp,ArTps),
  genVrs(ArTps,Lc,Args),
  concat(Cx,Args,NArgs),
  lcPk(Lc,Path),
  call(Make,Lc,OverOp,NArgs,Tp,Call),
  lambdaLbl(Path,"curry",Lbl).

genVrs([],_,[]).
genVrs([Tp|ArTps],Lc,[v(Lc,Id,Tp)|Vrs]) :-
  genstr("V",Id),
  genVrs(ArTps,Lc,Vrs).

overloadLst([],_,_,_,St,St,[]):-!.
overloadLst([T|L],C,D,O,St,Stx,[RT|RL]) :-
  call(C,T,D,O,St,St0,RT),
  overloadLst(L,C,D,O,St0,Stx,RL).

overloadList([],_,_,_,[]):-!.
overloadList([T|L],C,D,O,[RT|RL]) :-
  call(C,T,D,O,RT),
  overloadList(L,C,D,O,RL).

resolveRef(over(Lc,Trm,Con),DTs,Args,over(Lc,Over,Con),Dict,Opts,St,Stx,NArgs) :-
  resolveRef(Trm,DTs,Args,Over,Dict,Opts,St,Stx,NArgs).
resolveRef(mtd(Lc,Nm,Tp),[DT|Ds],RArgs,MtdCall,Dict,Opts,St,Stx,Args) :-
  concat(Ds,RArgs,Args),
  resolveDot(Lc,DT,Nm,Tp,Dict,Opts,St,Stx,MtdCall),!.
resolveRef(v(Lc,Nm,Tp),DT,RArgs,v(Lc,Nm,Tp),_,_,Stx,Stx,Args) :- !,
  concat(DT,RArgs,Args).
resolveRef(C,DT,RArgs,C,_,Opts,Stx,Stx,Args) :-
  [Cx|_]=DT,
  checkOpt(Opts,traceCheck,meta:showMsg(none,"resolve ref default %s",[Cx])),
  concat(DT,RArgs,Args).

resolveDot(Lc,Rc,Fld,Tp,Dict,Opts,St,Stx,Reslvd) :-
  typeOfCanon(Rc,RcTp),
  findAccess(RcTp,Fld,Dict,AccTp,FunNm),
  freshen(AccTp,Dict,_,FAccTp),
  newTypeVar("FF",RTp),
  (sameType(funType(tplType([RcTp]),RTp),FAccTp,Lc,Dict),
   freshen(RTp,Dict,_,CFTp),
   getConstraints(CFTp,_Cx,FTp), % constraints here are dropped!
   sameType(FTp,Tp,Lc,Dict),
   V = v(Lc,FunNm,funType(tplType([RcTp]),Tp)),
   Reslvd = apply(Lc,V,tple(Lc,[Rc]),Tp),
%   manageConstraints(Cx,Lc,apply(Lc,V,tple(Lc,[Rc]),Tp),Reslvd),
   traceCheck(Opts,Lc,"dot resolved term %s",[can(Reslvd)]),
   markResolved(St,Stx);
   genMsg("accessor defined for %s:%s in %s\nnot consistent with\n%s",
	  [Fld,tpe(FAccTp),can(dot(Lc,Rc,Fld,Tp)),tpe(Tp)],Msg),
   markFatal(St,Lc,Msg,Stx),
   Reslvd = dot(Lc,Rc,Fld,Tp)).
resolveDot(Lc,Rc,Fld,Tp,_Dict,_,St,Stx,dot(Lc,Rc,Fld,Tp)) :-
  typeOfCanon(Rc,RcTp),
  genMsg("no accessor defined for %s for type %s in %s",
	 [Fld,tpe(RcTp),can(dot(Lc,Rc,Fld,Tp))],Msg),
  markActive(St,Lc,Msg,Stx).

resolveTDot(Lc,Rc,Ix,Tp,Dict,_Opts,St,Stx,tdot(Lc,Rc,Ix,Tp)) :-
  typeOfCanon(Rc,RcTp),
  deRef(RcTp,tplType(Els)),!,
  (length(Els,Ar), Ar>Ix ->
   nth_of(Els,Ix,ElTp),
   (sameType(ElTp,Tp,Lc,Dict) -> St=Stx ;
    genMsg("%sth type of %s:%s not consistent with expected type %s",
	   [ix(Ix),tpe(ElTp),tpe(Tp)],Msg),
    markFatal(St,Lc,Msg,Stx));
   genMsg("%type of %s:%s not a tuple of at least %d",
	   [can(Rc),tpe(RcTp),ix(Ix)],Msg),
   markFatal(St,Lc,Msg,Stx)).
resolveTDot(Lc,Rc,Ix,Tp,_Dict,_,St,Stx,tdot(Lc,Rc,Ix,Tp)) :-
  typeOfCanon(Rc,RcTp),
  genMsg("%type of %s:%s not a tuple of at least %d",
	 [can(Rc),tpe(RcTp),ix(Ix)],Msg),
  markFatal(St,Lc,Msg,Stx).

resolveAccess(Lc,RcTp,Fld,Tp,Dict,_Opts,St,Stx,Reslvd) :-
  findAccess(RcTp,Fld,Dict,AccTp,FunNm),
  freshen(AccTp,Dict,_,FAccTp),
  newTypeVar("FF",RTp),
  (sameType(funType(tplType([RcTp]),RTp),FAccTp,Lc,Dict),
   freshen(RTp,Dict,_,CFTp),
   getConstraints(CFTp,Cx,FTp),
   sameType(FTp,Tp,Lc,Dict),
   V = v(Lc,FunNm,funType(tplType([RcTp]),FTp)),
   manageConstraints(Cx,Lc,V,Reslvd),
   markResolved(St,Stx);
   genMsg("no accessor for %s defined for type %s",[ss(Fld),tpe(RcTp)],Msg),
   Reslvd=void,
   markFatal(St,Lc,Msg,Stx)).
   
resolveUpdate(Lc,Rc,Fld,Vl,Dict,Opts,St,Stx,Reslvd) :-
  typeOfCanon(Rc,RcTp),
  typeOfCanon(Vl,VlTp),
  findUpdate(RcTp,Fld,Dict,AccTp,FunNm),
  freshen(AccTp,Dict,_,FAccTp),
  newTypeVar("FF",VTp),
  (sameType(funType(tplType([RcTp,VTp]),RcTp),FAccTp,Lc,Dict),
   freshen(VTp,Dict,_,CVTp),
   getConstraints(CVTp,Cx,FTp),
   sameType(FTp,VlTp,Lc,Dict),
   V = v(Lc,FunNm,funType(tplType([RcTp,VlTp]),RcTp)),
   Acc = apply(Lc,V,tple(Lc,[Rc,Vl]),RcTp),
   resolveConstraints(Lc,Cx,Dict,Opts,St,St0,DTerms),
   resolveRef(Acc,DTerms,[],OverOp,Dict,Opts,St0,St1,NArgs),
   overApply(Lc,OverOp,NArgs,RcTp,makeApply,Reslvd),
   markResolved(St1,Stx);
   genMsg("updater defined for %s:%s in %s\nnot consistent with\n%s",
	  [Fld,tpe(FAccTp),can(update(Lc,Rc,Fld,Vl)),tpe(RcTp)],Msg),
   markFatal(St,Lc,Msg,Stx),
   Reslvd = update(Lc,Rc,Fld,Vl)).
resolveUpdate(Lc,Rc,Fld,Vl,_Dict,_,St,Stx,update(Lc,Rc,Fld,Vl)) :-
  typeOfCanon(Rc,RcTp),
  genMsg("no updater defined for %s for type %s in %s",
	 [Fld,tpe(RcTp),can(upate(Lc,Rc,Fld,Vl))],Msg),
  markFatal(St,Lc,Msg,Stx).

resolveConstraints(_,[],_,_,St,St,[]).
resolveConstraints(Lc,[Con|C],Dict,Opts,St,Stx,Extra) :-
  resolveConstraint(Lc,Con,Dict,Opts,St,St0,Extra,Exs),!,
  resolveConstraints(Lc,C,Dict,Opts,St0,Stx,Exs).

resolveConstraint(Lc,implicit(Nm,Tp),Dict,Opts,St,Stx,[Over|Exs],Exs) :-
  resolveImplicit(Lc,Nm,Tp,Dict,Opts,St,Stx,Over).
resolveConstraint(Lc,C,Dict,Opts,St,Stx,[Over|Exs],Exs) :-
  implementationName(C,ImpNm),
  getImplementation(Dict,ImpNm,ImplVrNm,_ImplTp),
  getVar(Lc,ImplVrNm,Dict,Impl,ITp),
  contractType(C,CTp),
  sameType(ITp,CTp,Lc,Dict),
  markResolved(St,St1),
  overloadTerm(Impl,Dict,Opts,St1,Stx,Over).

resolveImplicit(Lc,Nm,Tp,Dict,Opts,St,Stx,Over) :-
  checkOpt(Opts,traceCheck,meta:showConstraint(Lc,"resolve %s",implicit(Nm,Tp))),
  (getVar(Lc,Nm,Dict,Over,ITp),
   (sameType(ITp,Tp,Lc,Dict),
    markResolved(St,Stx);
    genMsg("implicit %s:%s not consistent with %s",[Nm,tpe(ITp),tpe(Tp)],Msg),
    markFatal(St,Lc,Msg,Stx),
    Over=void) ;
   genMsg("implicit %s:%s not defined",[Nm,tpe(Tp)],Msg),
   markFatal(St,Lc,Msg,Stx),
   Over=void).

genVar(Nm,Lc,Tp,v(Lc,NV,Tp)) :-
  genstr(Nm,NV).

findAccess(Tp,FldNm,Dict,AccTp,FunNm) :-
  getFieldAccess(Tp,FldNm,FunNm,AccTp,Dict).
findAccess(_Tp,FldNm,Dict,AccTp,FunNm) :-
  is_member(access(FldNm,v(_,FunNm,AccTp)),Dict),!.

findUpdate(Tp,FldNm,Dict,AccTp,FunNm) :-
  getFieldUpdater(Tp,FldNm,FunNm,AccTp,Dict).
findUpdate(_Tp,FldNm,Dict,AccTp,FunNm) :-
  is_member(update(FldNm,v(_,FunNm,AccTp)),Dict),!.

resolveHead(Hd,[],Hd).
resolveHead(enm(Lc,Nm,Tp),CVars,apply(Lc,v(Lc,Nm,Tp),CVars)).
resolveHead(apply(Lc,v(ALc,Nm,Tp),Args,Tpx),CVars,apply(Lc,v(ALc,Nm,Tp),OArgs,Tpx)) :-
  addExtra(CVars,Args,OArgs).

addExtra(Extra,tple(Lc,Els),tple(Lc,EEls)) :-
  concat(Extra,Els,EEls).

addExtraDefs([],Els,Els).
addExtraDefs([v(Lc,Nm,Tp)|Ex],Els,REls) :-
  addExtraDefs(Ex,[varDef(Lc,Nm,Nm,[],Tp,v(Lc,Nm,Tp))|Els],REls).

traceCheck(Opts,Lc,Msg,Args) :-
  is_member(traceCheck,Opts),!,
  reportMsg(Msg,Args,Lc).
traceCheck(_,_,_,_).

